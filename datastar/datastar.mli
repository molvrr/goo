(** Datastar ({:https://data-star.dev}) helpers: attributes, action
    expressions, SSE events and signal reading. Targets Datastar {!version}. *)

val version : string
val js_file : string

type duration = [ `ms of int | `s of int ]

(** {1 Attributes}

    All attributes are valid on any element. Expression arguments are
    Datastar/JS expressions, inserted as written (and HTML-escaped). Signal
    names given as keys may be camelCase; they are written so Datastar reads
    them back unchanged. *)

module Attributes : sig
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

  (** {2 Signals} *)

  (** [signals ["count", "0"]] renders [data-signals="{'count': 0}"]. *)
  val signals : ?ifmissing:bool -> (string * string) list -> 'a Html.attr
  val signals_json : ?ifmissing:bool -> Yojson.Safe.t -> 'a Html.attr

  (** [signal "count" "0"] renders [data-signals:count="0"]. *)
  val signal : ?ifmissing:bool -> string -> string -> 'a Html.attr
  val computed : string -> string -> 'a Html.attr

  (** Two-way binding; [prop] and [event] override the bound property and
      the event that updates the signal. *)
  val bind : ?prop:string -> ?event:string -> string -> 'a Html.attr

  val indicator : string -> 'a Html.attr
  val ref_ : string -> 'a Html.attr
  val json_signals : ?terse:bool -> ?include_:string -> ?exclude:string -> unit -> 'a Html.attr

  (** {2 DOM} *)

  val text : string -> 'a Html.attr
  val show : string -> 'a Html.attr
  val attr : string -> string -> 'a Html.attr
  val attrs : (string * string) list -> 'a Html.attr
  val class_ : string -> string -> 'a Html.attr
  val classes : (string * string) list -> 'a Html.attr
  val style : string -> string -> 'a Html.attr
  val styles : (string * string) list -> 'a Html.attr

  (** {2 Effects and events} *)

  val effect_ : string -> 'a Html.attr
  val init : ?delay:duration -> ?viewtransition:bool -> string -> 'a Html.attr

  (** [on ~mods:[`prevent; `debounce (`ms 300)] "click" expr] renders
      [data-on:click__prevent__debounce.300ms="expr"]. *)
  val on : ?mods:[< on_modifier ] list -> string -> string -> 'a Html.attr

  val on_intersect : ?mods:[< intersect_modifier ] list -> string -> 'a Html.attr
  val on_interval : ?duration:duration -> ?leading:bool -> ?viewtransition:bool -> string -> 'a Html.attr
  val on_signal_patch : ?mods:[< timing ] list -> string -> 'a Html.attr
  val on_signal_patch_filter : ?include_:string -> ?exclude:string -> unit -> 'a Html.attr

  (** {2 Morphing} *)

  val ignore_ : 'a Html.attr
  val ignore_self : 'a Html.attr
  val ignore_morph : 'a Html.attr
  val preserve_attr : string list -> 'a Html.attr
end

(** {1 Actions}

    Build action expressions for use in attributes, e.g.
    [Attributes.on "click" (Actions.post "/save")]. [include_] and [exclude]
    are regular expressions (source text) filtering which signals are sent. *)

module Actions : sig
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

  val get : request
  val post : request
  val put : request
  val patch : request
  val delete : request

  val set_all : ?include_:string -> ?exclude:string -> string -> string
  val toggle_all : ?include_:string -> ?exclude:string -> unit -> string
end

(** {1 Server-sent events}

    Events are lists of SSE fields ([event], [id], [retry], [data]); send
    them with [Datastar_web.sse] (library [datastar.web]). *)

module Sse : sig
  type event = (string * string) list
  type mode = [ `outer | `inner | `replace | `prepend | `append | `before | `after | `remove ]
  type namespace = [ `html | `svg | `mathml ]

  val patch_elements :
    ?id:string -> ?retry:int -> ?selector:string -> ?mode:mode -> ?namespace:namespace ->
    ?use_view_transition:bool -> 'a Html.elt list -> event

  val remove_elements : ?id:string -> ?retry:int -> ?use_view_transition:bool -> string -> event

  val patch_signals : ?id:string -> ?retry:int -> ?only_if_missing:bool -> Yojson.Safe.t -> event

  (** Appends a [script] to [body]; by default it removes itself after
      running. [code] is inserted verbatim. *)
  val execute_script : ?id:string -> ?retry:int -> ?auto_remove:bool -> string -> event
end
