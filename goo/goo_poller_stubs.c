/* Readiness polling for Goo's schedulers: epoll on Linux, kqueue on the
   BSDs and macOS. Level-triggered: a ready fd keeps being reported while
   it's watched, and Goo stops watching it once nobody waits on it.

   One poller per scheduler (domain). Interest masks: 1 = read, 2 = write.
   [modify] is told what the fd had ([old]) so it can pick add / modify /
   delete, and tolerates the kernel having forgotten an fd that was closed
   behind Goo's back (ENOENT, EBADF on removal; ENOENT on modify -> add;
   EEXIST on add -> modify). Errors it can't absorb are raised as
   Unix_error, e.g. EPERM for a regular file, which epoll can't watch. */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <unistd.h>

#if defined(__linux__)
#define GOO_EPOLL 1
#include <sys/epoll.h>
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
#define GOO_KQUEUE 1
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/stat.h>
#endif

#define GOO_MAX_EVENTS 256

/* 0: none (use select), 1: epoll, 2: kqueue */
CAMLprim value goo_poller_kind(value unit)
{
  (void)unit;
#if defined(GOO_EPOLL)
  return Val_int(1);
#elif defined(GOO_KQUEUE)
  return Val_int(2);
#else
  return Val_int(0);
#endif
}

CAMLprim value goo_poller_create(value unit)
{
  (void)unit;
#if defined(GOO_EPOLL)
  int fd = epoll_create1(EPOLL_CLOEXEC);
  if (fd < 0) caml_uerror("epoll_create1", Nothing);
  return Val_int(fd);
#elif defined(GOO_KQUEUE)
  int fd = kqueue();
  if (fd < 0) caml_uerror("kqueue", Nothing);
  return Val_int(fd);
#else
  caml_unix_error(ENOSYS, "goo_poller_create", Nothing);
  return Val_unit;
#endif
}

CAMLprim value goo_poller_modify(value vp, value vfd, value vold, value vnew)
{
  int p = Int_val(vp), fd = Int_val(vfd), old = Int_val(vold), want = Int_val(vnew);
#if defined(GOO_EPOLL)
  if (want == 0) {
    if (old != 0 && epoll_ctl(p, EPOLL_CTL_DEL, fd, NULL) < 0 && errno != ENOENT && errno != EBADF)
      caml_uerror("epoll_ctl", Nothing);
    return Val_unit;
  }
  struct epoll_event ev;
  ev.events = 0;
  if (want & 1) ev.events |= EPOLLIN | EPOLLRDHUP;
  if (want & 2) ev.events |= EPOLLOUT;
  ev.data.u64 = 0;
  ev.data.fd = fd;
  int op = old != 0 ? EPOLL_CTL_MOD : EPOLL_CTL_ADD;
  if (epoll_ctl(p, op, fd, &ev) < 0) {
    int r = -1;
    if (op == EPOLL_CTL_MOD && errno == ENOENT) r = epoll_ctl(p, EPOLL_CTL_ADD, fd, &ev);
    else if (op == EPOLL_CTL_ADD && errno == EEXIST) r = epoll_ctl(p, EPOLL_CTL_MOD, fd, &ev);
    if (r < 0) caml_uerror("epoll_ctl", Nothing);
  }
  return Val_unit;
#elif defined(GOO_KQUEUE)
  /* Like epoll, refuse what's always ready to select (regular files,
     directories): kqueue would wait for data past their end instead. */
  if (old == 0 && want != 0) {
    struct stat st;
    if (fstat(fd, &st) == 0 && (S_ISREG(st.st_mode) || S_ISDIR(st.st_mode)))
      caml_unix_error(EPERM, "kevent", Nothing);
  }
  struct kevent ch[2], out[2];
  int n = 0, deleting[2];
  if (want & 1) { EV_SET(&ch[n], fd, EVFILT_READ, EV_ADD | EV_RECEIPT, 0, 0, 0); deleting[n++] = 0; }
  else if (old & 1) { EV_SET(&ch[n], fd, EVFILT_READ, EV_DELETE | EV_RECEIPT, 0, 0, 0); deleting[n++] = 1; }
  if (want & 2) { EV_SET(&ch[n], fd, EVFILT_WRITE, EV_ADD | EV_RECEIPT, 0, 0, 0); deleting[n++] = 0; }
  else if (old & 2) { EV_SET(&ch[n], fd, EVFILT_WRITE, EV_DELETE | EV_RECEIPT, 0, 0, 0); deleting[n++] = 1; }
  if (n == 0) return Val_unit;
  struct timespec zero = { 0, 0 };
  int r = kevent(p, ch, n, out, n, &zero);
  if (r < 0) caml_uerror("kevent", Nothing);
  for (int i = 0; i < r; i++) {
    if ((out[i].flags & EV_ERROR) && out[i].data != 0) {
      int err = (int)out[i].data;
      /* Which change it answers: receipts come back in order. */
      int del = i < n ? deleting[i] : 0;
      if (del && (err == ENOENT || err == EBADF)) continue;
      caml_unix_error(err, "kevent", Nothing);
    }
  }
  return Val_unit;
#else
  (void)p; (void)fd; (void)old; (void)want;
  caml_unix_error(ENOSYS, "goo_poller_modify", Nothing);
  return Val_unit;
#endif
}

/* Waits up to [timeout_ms] (-1: forever) and writes the ready fds into
   [buf] as pairs (fd, mask); returns how many pairs. EINTR is 0 events.
   The runtime is released while waiting, like Unix.select, so other
   domains can stop the world meanwhile. */
CAMLprim value goo_poller_wait(value vp, value vtimeout, value buf)
{
  CAMLparam1(buf);
  int p = Int_val(vp), timeout = Int_val(vtimeout);
  int cap = (int)(Wosize_val(buf) / 2);
  if (cap > GOO_MAX_EVENTS) cap = GOO_MAX_EVENTS;
  int n, err = 0;
#if defined(GOO_EPOLL)
  struct epoll_event evs[GOO_MAX_EVENTS];
  caml_enter_blocking_section();
  n = epoll_wait(p, evs, cap, timeout);
  if (n < 0) err = errno;
  caml_leave_blocking_section();
  if (n < 0) {
    if (err == EINTR) CAMLreturn(Val_int(0));
    caml_unix_error(err, "epoll_wait", Nothing);
  }
  for (int i = 0; i < n; i++) {
    uint32_t e = evs[i].events;
    int mask = 0;
    if (e & (EPOLLIN | EPOLLRDHUP | EPOLLHUP | EPOLLERR | EPOLLPRI)) mask |= 1;
    if (e & (EPOLLOUT | EPOLLHUP | EPOLLERR)) mask |= 2;
    Field(buf, 2 * i) = Val_int(evs[i].data.fd);
    Field(buf, 2 * i + 1) = Val_int(mask);
  }
#elif defined(GOO_KQUEUE)
  struct kevent evs[GOO_MAX_EVENTS];
  struct timespec ts, *tsp = NULL;
  if (timeout >= 0) { ts.tv_sec = timeout / 1000; ts.tv_nsec = (long)(timeout % 1000) * 1000000L; tsp = &ts; }
  caml_enter_blocking_section();
  n = kevent(p, NULL, 0, evs, cap, tsp);
  if (n < 0) err = errno;
  caml_leave_blocking_section();
  if (n < 0) {
    if (err == EINTR) CAMLreturn(Val_int(0));
    caml_unix_error(err, "kevent", Nothing);
  }
  int k = 0;
  for (int i = 0; i < n; i++) {
    int mask = 0;
    if (evs[i].filter == EVFILT_READ) mask = 1;
    else if (evs[i].filter == EVFILT_WRITE) mask = 2;
    if (evs[i].flags & EV_ERROR) mask = 3;
    if (mask == 0) continue;
    Field(buf, 2 * k) = Val_int((int)evs[i].ident);
    Field(buf, 2 * k + 1) = Val_int(mask);
    k++;
  }
  n = k;
#else
  (void)p; (void)timeout; (void)cap; (void)err;
  caml_unix_error(ENOSYS, "goo_poller_wait", Nothing);
  n = 0;
#endif
  CAMLreturn(Val_int(n));
}
