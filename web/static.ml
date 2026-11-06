(* {1 Content types} *)

let mime_types =
  [
    ("html", "text/html; charset=utf-8"); ("htm", "text/html; charset=utf-8");
    ("css", "text/css; charset=utf-8"); ("js", "text/javascript; charset=utf-8");
    ("mjs", "text/javascript; charset=utf-8"); ("json", "application/json");
    ("map", "application/json"); ("txt", "text/plain; charset=utf-8");
    ("md", "text/markdown; charset=utf-8"); ("csv", "text/csv; charset=utf-8");
    ("xml", "application/xml"); ("svg", "image/svg+xml"); ("png", "image/png");
    ("jpg", "image/jpeg"); ("jpeg", "image/jpeg"); ("gif", "image/gif"); ("webp", "image/webp");
    ("avif", "image/avif"); ("ico", "image/x-icon"); ("woff", "font/woff"); ("woff2", "font/woff2");
    ("ttf", "font/ttf"); ("otf", "font/otf"); ("wasm", "application/wasm"); ("pdf", "application/pdf");
    ("zip", "application/zip"); ("gz", "application/gzip"); ("mp4", "video/mp4"); ("webm", "video/webm");
    ("mp3", "audio/mpeg"); ("ogg", "audio/ogg"); ("wav", "audio/wav"); ("webmanifest", "application/manifest+json");
  ]

let mime_type name =
  match String.rindex_opt name '.' with
  | Some i -> (
      let ext = String.lowercase_ascii (String.sub name (i + 1) (String.length name - i - 1)) in
      match List.assoc_opt ext mime_types with Some t -> t | None -> "application/octet-stream")
  | None -> "application/octet-stream"

(* {1 HTTP dates} (IMF-fixdate, RFC 9110 5.6.7) *)

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let http_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" days.(tm.tm_wday) tm.tm_mday months.(tm.tm_mon)
    (tm.tm_year + 1900) tm.tm_hour tm.tm_min tm.tm_sec

(* Days since 1970-01-01 of a civil date (Howard Hinnant's algorithm): no
   timegm in Unix, and mktime is local time. *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let doy = ((153 * (if m > 2 then m - 3 else m + 9)) + 2) / 5 + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

(* Only IMF-fixdate; the obsolete formats count as absent (RFC 9110 lets a
   server ignore a date it can't parse). *)
let parse_http_date s =
  try
    Scanf.sscanf s "%3s, %2d %3s %4d %2d:%2d:%2d GMT%!" (fun _ d mon y h mi sec ->
        match Array.find_index (String.equal mon) months with
        | Some m -> Some (float ((((days_from_civil y (m + 1) d * 24) + h) * 60 + mi) * 60 + sec))
        | None -> None)
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* {1 Ranges} *)

(* A single "bytes=" range against [size]: [`Range (start, len)],
   [`Unsatisfiable], or [`Whole] when absent, malformed or multiple
   (serving the whole file is always allowed). *)
let parse_range size header =
  match header with
  | None -> `Whole
  | Some h -> (
      let h = String.trim h in
      if not (String.starts_with ~prefix:"bytes=" h) then `Whole
      else
        let spec = String.trim (String.sub h 6 (String.length h - 6)) in
        if String.contains spec ',' then `Whole
        else
          match String.index_opt spec '-' with
          | None -> `Whole
          | Some i -> (
              let digits s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s in
              let a = String.trim (String.sub spec 0 i)
              and b = String.trim (String.sub spec (i + 1) (String.length spec - i - 1)) in
              match (digits a, digits b) with
              | false, true -> (
                  (* Suffix: the last [b] bytes. *)
                  match int_of_string_opt b with
                  | Some n when n > 0 && size > 0 ->
                      let n = min n size in
                      `Range (size - n, n)
                  | Some _ -> `Unsatisfiable
                  | None -> `Whole)
              | true, (true | false) when digits a && (b = "" || digits b) -> (
                  match (int_of_string_opt a, if b = "" then Some (size - 1) else int_of_string_opt b) with
                  | Some first, Some last when first < size && first <= last ->
                      let last = min last (size - 1) in
                      `Range (first, last - first + 1)
                  | Some first, Some last when first <= last -> `Unsatisfiable
                  | Some first, Some _ when first >= size -> `Unsatisfiable
                  | _ -> `Whole)
              | _ -> `Whole))

(* {1 Serving} *)

let not_found () = Response.text ~status:`Not_found "Not Found"

(* [real] is inside [root] (both real paths). *)
let inside ~root real = real = root || String.starts_with ~prefix:(root ^ "/") real

let resolve ~follow_symlinks ~root path =
  match Unix.realpath path with
  | real when follow_symlinks = `Anywhere || inside ~root real -> Some real
  | _ -> None
  | exception Unix.Unix_error _ -> None

(* Open without blocking (a FIFO would block a blocking open) and keep it
   only if it is a regular file, checked on the descriptor itself. *)
let open_regular path =
  match Unix.openfile path [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0 with
  | exception Unix.Unix_error _ -> None
  | fd -> (
      match Unix.fstat fd with
      | { st_kind = S_REG; _ } as st ->
          Unix.clear_nonblock fd;
          Some (fd, st)
      | _ ->
          Unix.close fd;
          None
      | exception Unix.Unix_error _ ->
          Unix.close fd;
          None)

let etag (st : Unix.stats) = Printf.sprintf "W/\"%x-%x\"" st.st_size (int_of_float st.st_mtime)

(* If-None-Match takes precedence over If-Modified-Since (RFC 9110 13.2.2);
   weak comparison, as for GET. *)
let not_modified req ~etag ~mtime =
  match Request.header req "if-none-match" with
  | Some v ->
      let strip t = if String.starts_with ~prefix:"W/" t then String.sub t 2 (String.length t - 2) else t in
      let tags = List.map String.trim (String.split_on_char ',' v) in
      List.mem "*" tags || List.exists (fun t -> strip t = strip etag) tags
  | None -> (
      match Option.bind (Request.header req "if-modified-since") parse_http_date with
      | Some since -> Float.of_int (int_of_float mtime) <= since
      | None -> false)

let chunk_size = 65536

let send_file req ~cache_control ~name fd (st : Unix.stats) =
  (* Closed by the stream, or at the end of the request's task if the body
     is never sent (HEAD, 304, or the client went away). *)
  let closed = ref false in
  let close () =
    if not !closed then begin
      closed := true;
      Unix.close fd
    end
  in
  Goo.defer close;
  let size = st.st_size and etag = etag st and last_modified = http_date st.st_mtime in
  let base =
    [
      ("etag", etag);
      ("last-modified", last_modified);
      ("cache-control", cache_control);
      ("x-content-type-options", "nosniff");
    ]
  in
  if not_modified req ~etag ~mtime:st.st_mtime then begin
    close ();
    Response.make ~status:`Not_modified ~headers:base ""
  end
  else
    (* If-Range: only the exact Last-Modified keeps the range (our ETag is
       weak, which If-Range can't use). *)
    let range_header =
      match Request.header req "if-range" with
      | Some v when v <> last_modified -> None
      | _ -> Request.header req "range"
    in
    let headers = base @ [ ("content-type", mime_type name); ("accept-ranges", "bytes") ] in
    match parse_range size range_header with
    | `Unsatisfiable ->
        close ();
        Response.text ~status:`Range_not_satisfiable
          ~headers:[ ("content-range", Printf.sprintf "bytes */%d" size) ]
          "Range Not Satisfiable"
    | (`Whole | `Range _) as r ->
        let status, start, len, extra =
          match r with
          | `Whole -> (`OK, 0, size, [])
          | `Range (start, len) ->
              ( `Partial_content,
                start,
                len,
                [ ("content-range", Printf.sprintf "bytes %d-%d/%d" start (start + len - 1) size) ] )
        in
        let headers = headers @ extra @ [ ("content-length", string_of_int len) ] in
        if Request.meth req = `HEAD then begin
          close ();
          Response.make ~status ~headers ""
        end
        else
          Response.stream ~status ~headers (fun write ->
              Fun.protect ~finally:close (fun () ->
                  ignore (Unix.lseek fd start SEEK_SET);
                  let buf = Bytes.create chunk_size in
                  let rec loop remaining =
                    if remaining > 0 then
                      match Unix.read fd buf 0 (min chunk_size remaining) with
                      | 0 -> () (* shrank meanwhile: the client sees a short body *)
                      | n ->
                          write (Bytes.sub_string buf 0 n);
                          loop (remaining - n)
                  in
                  loop len))

let serve ?(index = Some "index.html") ?(cache_control = "no-cache") ?(follow_symlinks = `Inside) dir =
  let root = Unix.realpath dir in
  fun req segs ->
    (* A trailing slash asks for a directory. *)
    let dir_request, names =
      match List.rev segs with "" :: rev -> (true, List.rev rev) | _ -> (false, segs)
    in
    if not (List.for_all Codec.is_filename names) then not_found ()
    else
      let path = String.concat "/" (root :: names) in
      match resolve ~follow_symlinks ~root path with
      | None -> not_found ()
      | Some real -> (
          match Unix.stat real with
          | exception Unix.Unix_error _ -> not_found ()
          | { st_kind = S_DIR; _ } -> (
              if not dir_request then
                (* Redirect to the slash form: the path's segments are all
                   plain names, so this stays on this site. *)
                let qs =
                  match String.index_opt (Request.target req) '?' with
                  | Some i -> String.sub (Request.target req) i (String.length (Request.target req) - i)
                  | None -> ""
                in
                Response.redirect ~status:`Moved_permanently (Request.path req ^ "/" ^ qs)
              else
                match index with
                | None -> not_found ()
                | Some index -> (
                    match resolve ~follow_symlinks ~root (Filename.concat real index) with
                    | None -> not_found ()
                    | Some file -> (
                        match open_regular file with
                        | Some (fd, st) -> send_file req ~cache_control ~name:index fd st
                        | None -> not_found ())))
          | { st_kind = S_REG; _ } when not dir_request -> (
              match open_regular real with
              | Some (fd, st) -> send_file req ~cache_control ~name:(List.nth names (List.length names - 1)) fd st
              | None -> not_found ())
          | _ -> not_found ())
