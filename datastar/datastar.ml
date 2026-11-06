let version = "1.0.4"
let js_file =
  "https://cdn.jsdelivr.net/gh/starfederation/datastar@v" ^ version ^ "/bundles/datastar.js"

type duration = [ `ms of int | `s of int ]

let string_of_duration : duration -> string = function
  | `ms n -> string_of_int n ^ "ms"
  | `s n -> string_of_int n ^ "s"

(* JS source helpers. Strings are JSON-encoded, which is valid JS. *)
module Js = struct
  let string s = Yojson.Safe.to_string (`String s)
  let object_ fields =
    "{" ^ String.concat ", " (List.map (fun (k, v) -> string k ^ ": " ^ v) fields) ^ "}"
end

(* HTML lowercases attribute names, and Datastar camel-cases keys by
   default, so [fooBar] must be written as [foo-bar] to round-trip. *)
let kebab s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    if c >= 'A' && c <= 'Z' then begin
      Buffer.add_char buf '-';
      Buffer.add_char buf (Char.lowercase_ascii c)
    end else Buffer.add_char buf c) s;
  Buffer.contents buf

let filter_object ?include_ ?exclude () =
  Js.object_ (List.filter_map Fun.id
    [ Option.map (fun r -> ("include", Js.string r)) include_
    ; Option.map (fun r -> ("exclude", Js.string r)) exclude ])

module Attributes = struct
  type case = [ `camel | `kebab | `snake | `pascal ]

  type timing =
    [ `delay of duration
    | `debounce of duration
    | `debounce_with of duration * [ `leading | `notrailing ] list
    | `throttle of duration
    | `throttle_with of duration * [ `noleading | `trailing ] list ]

  type on_modifier =
    [ timing | `once | `passive | `capture | `prevent | `stop
    | `window | `document | `outside | `viewtransition | `case of case ]

  type intersect_modifier =
    [ timing | `once | `exit | `half | `full | `threshold of int | `viewtransition ]

  let string_of_case : case -> string = function
    | `camel -> "camel" | `kebab -> "kebab" | `snake -> "snake" | `pascal -> "pascal"

  let with_tags name d tags = String.concat "." (name :: string_of_duration d :: tags)

  let string_of_modifier = function
    | `once -> "once"
    | `passive -> "passive"
    | `capture -> "capture"
    | `prevent -> "prevent"
    | `stop -> "stop"
    | `window -> "window"
    | `document -> "document"
    | `outside -> "outside"
    | `viewtransition -> "viewtransition"
    | `exit -> "exit"
    | `half -> "half"
    | `full -> "full"
    | `ifmissing -> "ifmissing"
    | `terse -> "terse"
    | `case c -> "case." ^ string_of_case c
    | `threshold n -> "threshold." ^ string_of_int n
    | `prop p -> "prop." ^ p
    | `event e -> "event." ^ e
    | `delay d -> with_tags "delay" d []
    | `debounce d -> with_tags "debounce" d []
    | `debounce_with (d, tags) ->
        with_tags "debounce" d (List.map (function `leading -> "leading" | `notrailing -> "notrailing") tags)
    | `throttle d -> with_tags "throttle" d []
    | `throttle_with (d, tags) ->
        with_tags "throttle" d (List.map (function `noleading -> "noleading" | `trailing -> "trailing") tags)
    | `duration (d, leading) -> with_tags "duration" d (if leading then ["leading"] else [])

  let make ?(mods = []) name value =
    let suffix = String.concat "" (List.map (fun m -> "__" ^ string_of_modifier m) mods) in
    Html.Attributes.attr ("data-" ^ name ^ suffix) value

  let if_ cond m = if cond then [m] else []

  let signals ?(ifmissing = false) fields =
    make ~mods:(if_ ifmissing `ifmissing) "signals" (Js.object_ fields)
  let signals_json ?(ifmissing = false) json =
    make ~mods:(if_ ifmissing `ifmissing) "signals" (Yojson.Safe.to_string json)
  let signal ?(ifmissing = false) name expr =
    make ~mods:(if_ ifmissing `ifmissing) ("signals:" ^ kebab name) expr
  let computed name expr = make ("computed:" ^ kebab name) expr

  let bind ?prop ?event signal =
    let mods = List.filter_map Fun.id
      [ Option.map (fun p -> `prop p) prop; Option.map (fun e -> `event e) event ] in
    make ~mods "bind" signal

  let text expr = make "text" expr
  let show expr = make "show" expr
  let effect_ expr = make "effect" expr

  let init ?delay ?(viewtransition = false) expr =
    let mods = Option.to_list (Option.map (fun d -> `delay d) delay)
               @ if_ viewtransition `viewtransition in
    make ~mods "init" expr

  let attr name expr = make ("attr:" ^ name) expr
  let attrs fields = make "attr" (Js.object_ fields)
  let class_ name expr = make ("class:" ^ name) expr
  let classes fields = make "class" (Js.object_ fields)
  let style name expr = make ("style:" ^ name) expr
  let styles fields = make "style" (Js.object_ fields)

  let on ?mods event expr = make ?mods ("on:" ^ event) expr
  let on_intersect ?mods expr = make ?mods "on-intersect" expr

  let on_interval ?duration ?(leading = false) ?(viewtransition = false) expr =
    let mods = Option.to_list (Option.map (fun d -> `duration (d, leading)) duration)
               @ if_ viewtransition `viewtransition in
    make ~mods "on-interval" expr

  let on_signal_patch ?mods expr = make ?mods "on-signal-patch" expr
  let on_signal_patch_filter ?include_ ?exclude () =
    Html.Attributes.attr "data-on-signal-patch-filter" (filter_object ?include_ ?exclude ())

  let indicator signal = make "indicator" signal
  let ref_ signal = make "ref" signal

  let json_signals ?(terse = false) ?include_ ?exclude () =
    let value = match include_, exclude with
      | None, None -> ""
      | _ -> filter_object ?include_ ?exclude () in
    make ~mods:(if_ terse `terse) "json-signals" value

  let ignore_ = Html.Attributes.flag "data-ignore"
  let ignore_self = Html.Attributes.flag "data-ignore__self"
  let ignore_morph = Html.Attributes.flag "data-ignore-morph"
  let preserve_attr names = Html.Attributes.attr "data-preserve-attr" (String.concat " " names)
end

module Actions = struct
  type content_type = [ `json | `form ]
  type request_cancellation = [ `auto | `disabled ]
  type retry = [ `auto | `error | `always | `never ]

  type request =
    ?content_type:content_type ->
    ?selector:string ->
    ?headers:(string * string) list ->
    ?open_when_hidden:bool ->
    ?include_:string ->
    ?exclude:string ->
    ?payload:string ->
    ?request_cancellation:request_cancellation ->
    ?retry:retry ->
    ?retry_interval:int ->
    ?retry_max_count:int ->
    string ->
    string

  let string_of_content_type : content_type -> string = function
    | `json -> "json" | `form -> "form"
  let string_of_request_cancellation : request_cancellation -> string = function
    | `auto -> "auto" | `disabled -> "disabled"
  let string_of_retry : retry -> string = function
    | `auto -> "auto" | `error -> "error" | `always -> "always" | `never -> "never"

  let request meth : request = fun ?content_type ?selector ?headers ?open_when_hidden ?include_ ?exclude
      ?payload ?request_cancellation ?retry ?retry_interval ?retry_max_count (url : string) ->
    let opt key f v = Option.map (fun v -> (key, f v)) v in
    let fields = List.filter_map Fun.id
      [ opt "contentType" (fun c -> Js.string (string_of_content_type c)) content_type
      ; opt "selector" Js.string selector
      ; opt "headers" (fun hs -> Js.object_ (List.map (fun (k, v) -> (k, Js.string v)) hs)) headers
      ; opt "openWhenHidden" string_of_bool open_when_hidden
      ; (match include_, exclude with
         | None, None -> None
         | _ -> Some ("filterSignals", filter_object ?include_ ?exclude ()))
      ; opt "payload" Fun.id payload
      ; opt "requestCancellation" (fun r -> Js.string (string_of_request_cancellation r)) request_cancellation
      ; opt "retry" (fun r -> Js.string (string_of_retry r)) retry
      ; opt "retryInterval" string_of_int retry_interval
      ; opt "retryMaxCount" string_of_int retry_max_count ] in
    let args = Js.string url :: (if fields = [] then [] else [Js.object_ fields]) in
    "@" ^ meth ^ "(" ^ String.concat ", " args ^ ")"

  let get = request "get"
  let post = request "post"
  let put = request "put"
  let patch = request "patch"
  let delete = request "delete"

  let filter_arg ?include_ ?exclude () =
    match include_, exclude with
    | None, None -> []
    | _ -> [filter_object ?include_ ?exclude ()]

  let set_all ?include_ ?exclude value =
    "@setAll(" ^ String.concat ", " (value :: filter_arg ?include_ ?exclude ()) ^ ")"
  let toggle_all ?include_ ?exclude () =
    "@toggleAll(" ^ String.concat ", " (filter_arg ?include_ ?exclude ()) ^ ")"
end

module Sse = struct
  type event = (string * string) list
  type mode = [ `outer | `inner | `replace | `prepend | `append | `before | `after | `remove ]
  type namespace = [ `html | `svg | `mathml ]

  let string_of_mode : mode -> string = function
    | `outer -> "outer" | `inner -> "inner" | `replace -> "replace" | `prepend -> "prepend"
    | `append -> "append" | `before -> "before" | `after -> "after" | `remove -> "remove"
  let string_of_namespace : namespace -> string = function
    | `html -> "html" | `svg -> "svg" | `mathml -> "mathml"

  (* SSE values cannot contain newlines: each line becomes its own data line. *)
  let data key value =
    if value = "" then []
    else List.map (fun line -> ("data", key ^ " " ^ line)) (String.split_on_char '\n' value)

  let header ?id ?retry name =
    ("event", name)
    :: List.filter_map Fun.id
         [ Option.map (fun id -> ("id", id)) id
         ; Option.map (fun ms -> ("retry", string_of_int ms)) retry ]

  let patch_elements ?id ?retry ?selector ?mode ?namespace ?(use_view_transition = false) elements =
    header ?id ?retry "datastar-patch-elements"
    @ (match selector with Some s -> data "selector" s | None -> [])
    @ (match mode with Some m -> data "mode" (string_of_mode m) | None -> [])
    @ (match namespace with Some n -> data "namespace" (string_of_namespace n) | None -> [])
    @ (if use_view_transition then data "useViewTransition" "true" else [])
    @ data "elements" (String.concat "" (List.map Html.render elements))

  let remove_elements ?id ?retry ?use_view_transition selector =
    patch_elements ?id ?retry ?use_view_transition ~selector ~mode:`remove []

  let patch_signals ?id ?retry ?(only_if_missing = false) signals =
    header ?id ?retry "datastar-patch-signals"
    @ (if only_if_missing then data "onlyIfMissing" "true" else [])
    @ data "signals" (Yojson.Safe.to_string signals)

  let execute_script ?id ?retry ?(auto_remove = true) code =
    let attrs = if auto_remove then [Html.Attributes.attr "data-effect" "el.remove()"] else [] in
    patch_elements ?id ?retry ~selector:"body" ~mode:`append [Html.script attrs [Html.raw code]]
end
