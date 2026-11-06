(** HTML builder with phantom types.

    - ['a attr]: ['a] is the attribute's name (e.g. [`href]). Each element
      lists the attribute names it accepts, so [div [Attributes.href "x"] []]
      is a type error.
    - ['a elt]: ['a] is the element's name (e.g. [`li]). Each element lists
      which children it accepts, following the HTML content categories, so
      [ul [] [p [] []]] is a type error.

    Simplifications: transparent elements ([a], [ins], [del], [audio],
    [video], [canvas], [map]) accept flow content, and interactive-content
    nesting rules (no [button] inside [a], etc.) are not enforced. *)

type +'a attr
type +'a elt
type document

(** {1 Attribute sets} *)

type global =
  [ `id | `class_ | `style | `title | `lang | `dir | `tabindex | `hidden | `inert
  | `autofocus | `role | `aria | `data | `accesskey | `contenteditable
  | `draggable | `spellcheck | `translate | `inputmode | `enterkeyhint
  | `popover ]

type form_control = [ global | `name | `disabled | `form ]

type media_attrs =
  [ global | `src | `crossorigin | `controls | `autoplay | `loop | `muted | `preload ]

(** {1 Content categories} *)

type metadata =
  [ `base | `link | `meta | `noscript | `script | `style | `template | `title ]

type script_supporting = [ `script | `template ]

type heading = [ `h1 | `h2 | `h3 | `h4 | `h5 | `h6 ]

type phrasing =
  [ `text | `raw | `a | `abbr | `audio | `b | `bdi | `bdo | `br | `button | `canvas
  | `cite | `code | `data | `datalist | `del | `em | `embed | `i | `iframe | `img
  | `input | `ins | `kbd | `label | `link | `map | `mark | `meta | `meter | `noscript
  | `output | `picture | `progress | `q | `ruby | `s | `samp | `script | `select
  | `slot | `small | `span | `strong | `sub | `sup | `svg | `template | `textarea
  | `time | `u | `var | `video | `wbr ]

type flow =
  [ phrasing | heading
  | `address | `article | `aside | `blockquote | `details | `dialog | `div | `dl
  | `fieldset | `figure | `footer | `form | `header | `hgroup | `hr | `main
  | `menu | `nav | `ol | `p | `pre | `search | `section | `style | `table | `ul ]

(** {1 Rendering} *)

(** Text content; [&], [<], [>] and both quote characters are escaped. *)
val text : string -> [> `text] elt

(** Trusted markup, inserted verbatim. Use for inline [script]/[style]
    bodies (which must not be entity-escaped) and SVG markup. Never pass
    untrusted input. *)
val raw : string -> [> `raw] elt

(** Escape [&], [<], [>] and both quote characters as HTML entities. *)
val escape : string -> string

val render : 'a elt -> string
val document : [< `head] elt -> [< `body] elt -> document
val render_document : document -> string

(** {1 Attributes} *)

module Attributes : sig
  type rel =
    [ `alternate
    | `author
    | `bookmark
    | `canonical
    | `dns_prefetch
    | `external_
    | `help
    | `icon
    | `license
    | `manifest
    | `me
    | `modulepreload
    | `next
    | `nofollow
    | `noopener
    | `noreferrer
    | `opener
    | `pingback
    | `preconnect
    | `prefetch
    | `preload
    | `prev
    | `search
    | `stylesheet
    | `tag
    | `other of string ]

  type target =
    [ `blank
    | `self
    | `parent
    | `top
    | `frame of string ]

  type referrerpolicy =
    [ `no_referrer
    | `no_referrer_when_downgrade
    | `origin
    | `origin_when_cross_origin
    | `same_origin
    | `strict_origin
    | `strict_origin_when_cross_origin
    | `unsafe_url ]

  type crossorigin =
    [ `anonymous
    | `use_credentials ]

  type loading =
    [ `eager
    | `lazy_ ]

  type form_method =
    [ `get
    | `post
    | `dialog ]

  type enctype =
    [ `urlencoded
    | `multipart
    | `text_plain ]

  type dir =
    [ `ltr
    | `rtl
    | `auto ]

  type wrap =
    [ `hard
    | `soft ]

  type scope =
    [ `row
    | `col
    | `rowgroup
    | `colgroup ]

  type track_kind =
    [ `subtitles
    | `captions
    | `descriptions
    | `chapters
    | `metadata ]

  type preload =
    [ `none
    | `metadata
    | `auto ]

  type inputmode =
    [ `none
    | `text
    | `decimal
    | `numeric
    | `tel
    | `search
    | `email
    | `url ]

  type enterkeyhint =
    [ `enter
    | `done_
    | `go
    | `next
    | `previous
    | `search
    | `send ]

  type popover =
    [ `auto
    | `manual
    | `hint ]

  type autocomplete =
    [ `on
    | `off
    | `name
    | `given_name
    | `family_name
    | `email
    | `username
    | `new_password
    | `current_password
    | `one_time_code
    | `tel
    | `url
    | `street_address
    | `postal_code
    | `country
    | `organization
    | `bday
    | `other of string ]

  type http_equiv =
    [ `content_security_policy
    | `content_type
    | `default_style
    | `refresh
    | `x_ua_compatible ]

  type sandbox =
    [ `allow_downloads
    | `allow_forms
    | `allow_modals
    | `allow_orientation_lock
    | `allow_pointer_lock
    | `allow_popups
    | `allow_popups_to_escape_sandbox
    | `allow_presentation
    | `allow_same_origin
    | `allow_scripts
    | `allow_top_navigation
    | `allow_top_navigation_by_user_activation
    | `allow_top_navigation_to_custom_protocols ]

  type input_type =
    [ `button
    | `checkbox
    | `color
    | `date
    | `datetime_local
    | `email
    | `file
    | `hidden
    | `image
    | `month
    | `number
    | `password
    | `radio
    | `range
    | `reset
    | `search
    | `submit
    | `tel
    | `text
    | `time
    | `url
    | `week ]

  type button_type =
    [ `submit
    | `reset
    | `button ]

  type script_type =
    [ `module_
    | `importmap
    | `speculationrules
    | `mime of string ]

  type charset =
    [ `utf_8 ]

  (** Escape hatches: valid on any element. *)

  val attr : string -> string -> 'a attr
  val flag : string -> 'a attr

  (** [data "foo" v] renders [data-foo="v"]. *)
  val data : string -> string -> [> `data] attr

  (** [aria "label" v] renders [aria-label="v"]. *)
  val aria : string -> string -> [> `aria] attr

  (** Ids and classes are {!Css.Id.t} / {!Css.Class.t}, shared with
      stylesheets. Use [attr "id" s] for an untyped one. *)
  val id : Css.Id.t -> [> `id] attr
  val class_ : Css.Class.t -> [> `class_] attr
  val classes : Css.Class.t list -> [> `class_] attr
  val style : Css.declaration list -> [> `style] attr
  val title : string -> [> `title] attr
  val lang : string -> [> `lang] attr
  val dir : dir -> [> `dir] attr
  val tabindex : int -> [> `tabindex] attr
  val hidden : [> `hidden] attr
  val inert : [> `inert] attr
  val autofocus : [> `autofocus] attr
  val role : string -> [> `role] attr
  val accesskey : string -> [> `accesskey] attr
  val contenteditable : bool -> [> `contenteditable] attr
  val draggable : bool -> [> `draggable] attr
  val spellcheck : bool -> [> `spellcheck] attr
  val translate : bool -> [> `translate] attr
  val inputmode : inputmode -> [> `inputmode] attr
  val enterkeyhint : enterkeyhint -> [> `enterkeyhint] attr
  val popover : popover -> [> `popover] attr
  val href : string -> [> `href] attr
  val target : target -> [> `target] attr
  val download : [> `download] attr
  val download_as : string -> [> `download] attr
  val rel : rel -> [> `rel] attr
  val rels : rel list -> [> `rel] attr
  val hreflang : string -> [> `hreflang] attr
  val referrerpolicy : referrerpolicy -> [> `referrerpolicy] attr
  val mime_type : string -> [> `mime_type] attr
  val src : string -> [> `src] attr
  val srcset : string -> [> `srcset] attr
  val sizes : string -> [> `sizes] attr
  val alt : string -> [> `alt] attr
  val width : int -> [> `width] attr
  val height : int -> [> `height] attr
  val loading : loading -> [> `loading] attr
  val crossorigin : crossorigin -> [> `crossorigin] attr
  val integrity : string -> [> `integrity] attr
  val onload : string -> [> `onload] attr
  val poster : string -> [> `poster] attr
  val preload : preload -> [> `preload] attr
  val controls : [> `controls] attr
  val autoplay : [> `autoplay] attr
  val loop : [> `loop] attr
  val muted : [> `muted] attr
  val playsinline : [> `playsinline] attr
  val kind : track_kind -> [> `kind] attr
  val srclang : string -> [> `srclang] attr
  val label : string -> [> `label] attr
  val default : [> `default] attr
  val sandbox : sandbox list -> [> `sandbox] attr
  val allow : string -> [> `allow] attr
  val srcdoc : string -> [> `srcdoc] attr
  val script_type : script_type -> [> `script_type] attr
  val async : [> `async] attr
  val defer : [> `defer] attr
  val nomodule : [> `nomodule] attr
  val charset : charset -> [> `charset] attr
  val name : string -> [> `name] attr
  val content : string -> [> `content] attr
  val http_equiv : http_equiv -> [> `http_equiv] attr
  val media : string -> [> `media] attr
  val action : string -> [> `action] attr
  val method_ : form_method -> [> `method_] attr
  val enctype : enctype -> [> `enctype] attr
  val novalidate : [> `novalidate] attr
  val accept_charset : charset -> [> `accept_charset] attr
  val autocomplete : autocomplete -> [> `autocomplete] attr
  val form : string -> [> `form] attr
  val formaction : string -> [> `formaction] attr
  val formmethod : form_method -> [> `formmethod] attr
  val formnovalidate : [> `formnovalidate] attr
  val value : string -> [> `value] attr
  val disabled : [> `disabled] attr
  val checked : [> `checked] attr
  val selected : [> `selected] attr
  val multiple : [> `multiple] attr
  val readonly : [> `readonly] attr
  val required : [> `required] attr
  val input_type : input_type -> [> `input_type] attr
  val button_type : button_type -> [> `button_type] attr
  val accept : string list -> [> `accept] attr
  val list : string -> [> `list] attr
  val min : string -> [> `min] attr
  val max : string -> [> `max] attr
  val step : string -> [> `step] attr
  val maxlength : int -> [> `maxlength] attr
  val minlength : int -> [> `minlength] attr
  val size : int -> [> `size] attr
  val pattern : string -> [> `pattern] attr
  val placeholder : string -> [> `placeholder] attr
  val for_ : string -> [> `for_] attr
  val rows : int -> [> `rows] attr
  val cols : int -> [> `cols] attr
  val wrap : wrap -> [> `wrap] attr
  val popovertarget : string -> [> `popovertarget] attr
  val colspan : int -> [> `colspan] attr
  val rowspan : int -> [> `rowspan] attr
  val span : int -> [> `span] attr
  val headers : string list -> [> `headers] attr
  val scope : scope -> [> `scope] attr
  val start : int -> [> `start] attr
  val reversed : [> `reversed] attr
  val cite : string -> [> `cite] attr
  val datetime : string -> [> `datetime] attr
  val open_ : [> `open_] attr
end

(** {1 Elements} *)

val html : [< global] attr list -> [< `head | `body] elt list -> [> `html] elt
val head : [< global] attr list -> [< metadata] elt list -> [> `head] elt
val body : [< global] attr list -> [< flow] elt list -> [> `body] elt
val title : [< global] attr list -> [< `text] elt list -> [> `title] elt
val base : [< global | `href | `target] attr list -> [> `base] elt
val link : [< global | `href | `rel | `hreflang | `media | `mime_type | `sizes | `crossorigin | `integrity | `referrerpolicy | `disabled] attr list -> [> `link] elt
val meta : [< global | `charset | `name | `content | `http_equiv | `media] attr list -> [> `meta] elt
val style : [< global | `media] attr list -> [< `raw] elt list -> [> `style] elt
val script : [< global | `src | `script_type | `async | `defer | `nomodule | `crossorigin | `integrity | `referrerpolicy] attr list -> [< `raw] elt list -> [> `script] elt
val noscript : [< global] attr list -> [< flow | metadata] elt list -> [> `noscript] elt
val template : [< global] attr list -> [< flow | metadata] elt list -> [> `template] elt
val main : [< global] attr list -> [< flow] elt list -> [> `main] elt
val header : [< global] attr list -> [< flow] elt list -> [> `header] elt
val footer : [< global] attr list -> [< flow] elt list -> [> `footer] elt
val nav : [< global] attr list -> [< flow] elt list -> [> `nav] elt
val section : [< global] attr list -> [< flow] elt list -> [> `section] elt
val article : [< global] attr list -> [< flow] elt list -> [> `article] elt
val aside : [< global] attr list -> [< flow] elt list -> [> `aside] elt
val address : [< global] attr list -> [< flow] elt list -> [> `address] elt
val search : [< global] attr list -> [< flow] elt list -> [> `search] elt
val h1 : [< global] attr list -> [< phrasing] elt list -> [> `h1] elt
val h2 : [< global] attr list -> [< phrasing] elt list -> [> `h2] elt
val h3 : [< global] attr list -> [< phrasing] elt list -> [> `h3] elt
val h4 : [< global] attr list -> [< phrasing] elt list -> [> `h4] elt
val h5 : [< global] attr list -> [< phrasing] elt list -> [> `h5] elt
val h6 : [< global] attr list -> [< phrasing] elt list -> [> `h6] elt
val hgroup : [< global] attr list -> [< heading | `p | script_supporting] elt list -> [> `hgroup] elt
val div : [< global] attr list -> [< flow] elt list -> [> `div] elt
val p : [< global] attr list -> [< phrasing] elt list -> [> `p] elt
val pre : [< global] attr list -> [< phrasing] elt list -> [> `pre] elt
val blockquote : [< global | `cite] attr list -> [< flow] elt list -> [> `blockquote] elt
val figure : [< global] attr list -> [< flow | `figcaption] elt list -> [> `figure] elt
val figcaption : [< global] attr list -> [< flow] elt list -> [> `figcaption] elt
val hr : [< global] attr list -> [> `hr] elt
val ul : [< global] attr list -> [< `li | script_supporting] elt list -> [> `ul] elt
val menu : [< global] attr list -> [< `li | script_supporting] elt list -> [> `menu] elt
val ol : [< global | `start | `reversed] attr list -> [< `li | script_supporting] elt list -> [> `ol] elt
val li : [< global | `value] attr list -> [< flow] elt list -> [> `li] elt
val dl : [< global] attr list -> [< `dt | `dd | `div | script_supporting] elt list -> [> `dl] elt
val dt : [< global] attr list -> [< flow] elt list -> [> `dt] elt
val dd : [< global] attr list -> [< flow] elt list -> [> `dd] elt
val a : [< global | `href | `target | `download | `rel | `hreflang | `referrerpolicy | `mime_type] attr list -> [< flow] elt list -> [> `a] elt
val span : [< global] attr list -> [< phrasing] elt list -> [> `span] elt
val strong : [< global] attr list -> [< phrasing] elt list -> [> `strong] elt
val em : [< global] attr list -> [< phrasing] elt list -> [> `em] elt
val b : [< global] attr list -> [< phrasing] elt list -> [> `b] elt
val i : [< global] attr list -> [< phrasing] elt list -> [> `i] elt
val u : [< global] attr list -> [< phrasing] elt list -> [> `u] elt
val s : [< global] attr list -> [< phrasing] elt list -> [> `s] elt
val small : [< global] attr list -> [< phrasing] elt list -> [> `small] elt
val mark : [< global] attr list -> [< phrasing] elt list -> [> `mark] elt
val code : [< global] attr list -> [< phrasing] elt list -> [> `code] elt
val kbd : [< global] attr list -> [< phrasing] elt list -> [> `kbd] elt
val samp : [< global] attr list -> [< phrasing] elt list -> [> `samp] elt
val var : [< global] attr list -> [< phrasing] elt list -> [> `var] elt
val sub : [< global] attr list -> [< phrasing] elt list -> [> `sub] elt
val sup : [< global] attr list -> [< phrasing] elt list -> [> `sup] elt
val abbr : [< global] attr list -> [< phrasing] elt list -> [> `abbr] elt
val bdi : [< global] attr list -> [< phrasing] elt list -> [> `bdi] elt
val bdo : [< global] attr list -> [< phrasing] elt list -> [> `bdo] elt
val cite : [< global] attr list -> [< phrasing] elt list -> [> `cite] elt
val q : [< global | `cite] attr list -> [< phrasing] elt list -> [> `q] elt
val time : [< global | `datetime] attr list -> [< phrasing] elt list -> [> `time] elt
val data : [< global | `value] attr list -> [< phrasing] elt list -> [> `data] elt
val del : [< global | `cite | `datetime] attr list -> [< flow] elt list -> [> `del] elt
val ins : [< global | `cite | `datetime] attr list -> [< flow] elt list -> [> `ins] elt
val ruby : [< global] attr list -> [< phrasing | `rt | `rp] elt list -> [> `ruby] elt
val rt : [< global] attr list -> [< phrasing] elt list -> [> `rt] elt
val rp : [< global] attr list -> [< phrasing] elt list -> [> `rp] elt
val br : [< global] attr list -> [> `br] elt
val wbr : [< global] attr list -> [> `wbr] elt
val form : [< global | `action | `method_ | `enctype | `novalidate | `accept_charset | `autocomplete | `target | `rel | `name] attr list -> [< flow] elt list -> [> `form] elt
val label : [< global | `for_] attr list -> [< phrasing] elt list -> [> `label] elt
val button : [< form_control | `button_type | `value | `formaction | `formmethod | `formnovalidate | `popovertarget] attr list -> [< phrasing] elt list -> [> `button] elt
val input : [< form_control | `input_type | `value | `formaction | `formmethod | `formnovalidate | `popovertarget | `accept | `alt | `autocomplete | `checked | `list | `max | `maxlength | `min | `minlength | `multiple | `pattern | `placeholder | `readonly | `required | `size | `src | `step | `width | `height] attr list -> [> `input] elt
val select : [< form_control | `autocomplete | `multiple | `required | `size] attr list -> [< `option | `optgroup | `hr | script_supporting] elt list -> [> `select] elt
val optgroup : [< global | `disabled | `label] attr list -> [< `option | script_supporting] elt list -> [> `optgroup] elt
val option : [< global | `disabled | `label | `selected | `value] attr list -> [< `text] elt list -> [> `option] elt
val textarea : [< form_control | `autocomplete | `cols | `rows | `wrap | `maxlength | `minlength | `placeholder | `readonly | `required] attr list -> [< `text] elt list -> [> `textarea] elt
val fieldset : [< form_control] attr list -> [< flow | `legend] elt list -> [> `fieldset] elt
val legend : [< global] attr list -> [< phrasing | heading] elt list -> [> `legend] elt
val output : [< global | `for_ | `form | `name] attr list -> [< phrasing] elt list -> [> `output] elt
val datalist : [< global] attr list -> [< phrasing | `option] elt list -> [> `datalist] elt
val progress : [< global | `value | `max] attr list -> [< phrasing] elt list -> [> `progress] elt
val meter : [< global | `value | `min | `max] attr list -> [< phrasing] elt list -> [> `meter] elt
val table : [< global] attr list -> [< `caption | `colgroup | `thead | `tbody | `tfoot | `tr | script_supporting] elt list -> [> `table] elt
val caption : [< global] attr list -> [< flow] elt list -> [> `caption] elt
val colgroup : [< global | `span] attr list -> [< `col | `template] elt list -> [> `colgroup] elt
val col : [< global | `span] attr list -> [> `col] elt
val thead : [< global] attr list -> [< `tr | script_supporting] elt list -> [> `thead] elt
val tbody : [< global] attr list -> [< `tr | script_supporting] elt list -> [> `tbody] elt
val tfoot : [< global] attr list -> [< `tr | script_supporting] elt list -> [> `tfoot] elt
val tr : [< global] attr list -> [< `td | `th | script_supporting] elt list -> [> `tr] elt
val th : [< global | `colspan | `rowspan | `headers | `scope] attr list -> [< flow] elt list -> [> `th] elt
val td : [< global | `colspan | `rowspan | `headers] attr list -> [< flow] elt list -> [> `td] elt
val img : [< global | `src | `alt | `srcset | `sizes | `width | `height | `loading | `crossorigin | `referrerpolicy] attr list -> [> `img] elt
val picture : [< global] attr list -> [< `source | `img | script_supporting] elt list -> [> `picture] elt
val source : [< global | `src | `srcset | `sizes | `media | `mime_type | `width | `height] attr list -> [> `source] elt
val video : [< media_attrs | `poster | `playsinline | `width | `height] attr list -> [< flow | `source | `track] elt list -> [> `video] elt
val audio : [< media_attrs] attr list -> [< flow | `source | `track] elt list -> [> `audio] elt
val track : [< global | `kind | `src | `srclang | `label | `default] attr list -> [> `track] elt
val iframe : [< global | `src | `srcdoc | `name | `sandbox | `allow | `width | `height | `loading | `referrerpolicy | `onload] attr list -> [> `iframe] elt
val embed : [< global | `src | `mime_type | `width | `height] attr list -> [> `embed] elt
val canvas : [< global | `width | `height] attr list -> [< flow] elt list -> [> `canvas] elt
val svg : [< global | `width | `height] attr list -> [< `raw] elt list -> [> `svg] elt
val map : [< global | `name] attr list -> [< flow | `area] elt list -> [> `map] elt
val area : [< global | `alt | `href | `target | `download | `rel | `referrerpolicy] attr list -> [> `area] elt
val details : [< global | `open_ | `name] attr list -> [< flow | `summary] elt list -> [> `details] elt
val summary : [< global] attr list -> [< phrasing | heading] elt list -> [> `summary] elt
val dialog : [< global | `open_] attr list -> [< flow] elt list -> [> `dialog] elt
val slot : [< global | `name] attr list -> [< flow] elt list -> [> `slot] elt
