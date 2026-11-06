(** Serving files from a directory.

    {[
      Web.App.route Web.Route.(get (s "assets" /? rest @-> Web.Static.serve "./public")) app
    ]}

    Only files under the directory are reachable: every requested segment
    must be a plain file name ({!Route.filename}: no [".."], ["/"] from
    [%2F], ["\\"], NUL, control characters or leading ["."]), the file's
    real path (symlinks resolved) must stay inside the directory, and only
    regular files are served (no devices or FIFOs, which are never opened in
    blocking mode). Anything else is a 404, whatever the reason, so the
    response doesn't reveal what exists.

    A directory answers with its [index] file; asked for without a trailing
    slash it redirects to it first, so relative links resolve. There are no
    directory listings.

    Responses carry a [content-type] (from the extension, see {!mime_type}),
    [content-length], [last-modified], a weak [etag], [cache-control] and
    [x-content-type-options: nosniff]. [If-None-Match] and
    [If-Modified-Since] answer 304; a single [Range] answers 206 (several
    ranges get the whole file, as RFC 9110 allows), an unsatisfiable one 416.
    HEAD works through the GET route.

    Files are read with blocking calls, a chunk at a time: fine from the page
    cache, but a slow disk stalls the domain while it reads. *)

val serve :
  ?index:string option ->
  ?cache_control:string ->
  ?follow_symlinks:[ `Inside | `Anywhere ] ->
  string ->
  Request.t ->
  string list ->
  Response.t
(** [serve dir] is a handler for a {!Route.rest} route serving [dir].

    @param index served for a directory (default [Some "index.html"];
    [None]: directories are 404s).
    @param cache_control default ["no-cache"]: browsers keep the file but
    revalidate it each time, which the ETag makes a cheap 304. Use e.g.
    ["public, max-age=31536000, immutable"] for content-hashed names.
    @param follow_symlinks [`Inside] (default): a symlink may only lead to
    a file inside [dir]. [`Anywhere] follows any symlink, e.g. for a
    directory of links into the Nix store.
    @raise Unix.Unix_error at once if [dir] doesn't exist. *)

val mime_type : string -> string
(** The content type for a file name, by extension; text types are
    UTF-8. Unknown: [application/octet-stream]. *)
