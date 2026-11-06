type 'a attr = string * string option

type node =
  | VoidTag of {name: string; attributes: (string * string option) list}
  | Tag of {name: string; attributes: (string * string option) list; children: node list}
  | Text of string
  | Raw of string

type 'a elt = node

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

type document = {metadata: [`head] elt; body: [`body] elt}
let document metadata body = {metadata; body}

let void_tag name attributes = VoidTag {name; attributes}
let tag name attributes children = Tag {name; attributes; children}
let text str = Text str
let raw str = Raw str

let escape_into buf s =
  String.iter (function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c -> Buffer.add_char buf c) s

let escape s =
  let buf = Buffer.create (String.length s) in
  escape_into buf s;
  Buffer.contents buf

let render_attributes buf attributes =
  List.iter (fun (key, value) ->
    Buffer.add_string buf " ";
    Buffer.add_string buf key;
    Option.iter (fun v ->
      Buffer.add_string buf "=\"";
      escape_into buf v;
      Buffer.add_string buf "\"") value
  ) attributes

let render node =
  let buf = Buffer.create 1024 in
  let rec go = function
    | Tag {name; attributes; children} ->
        Buffer.add_string buf "<";
        Buffer.add_string buf name;
        render_attributes buf attributes;
        Buffer.add_string buf ">";
        List.iter go children;
        Buffer.add_string buf "</";
        Buffer.add_string buf name;
        Buffer.add_string buf ">"
    | VoidTag {name; attributes} ->
        Buffer.add_string buf "<";
        Buffer.add_string buf name;
        render_attributes buf attributes;
        Buffer.add_string buf ">"
    | Text str -> escape_into buf str
    | Raw str -> Buffer.add_string buf str
  in
  go node;
  Buffer.contents buf

let render_document {metadata; body} =
  "<!doctype html>\n" ^ render metadata ^ render body

module Attributes = struct
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

  let string_of_rel : rel -> string = function
    | `alternate -> "alternate"
    | `author -> "author"
    | `bookmark -> "bookmark"
    | `canonical -> "canonical"
    | `dns_prefetch -> "dns-prefetch"
    | `external_ -> "external"
    | `help -> "help"
    | `icon -> "icon"
    | `license -> "license"
    | `manifest -> "manifest"
    | `me -> "me"
    | `modulepreload -> "modulepreload"
    | `next -> "next"
    | `nofollow -> "nofollow"
    | `noopener -> "noopener"
    | `noreferrer -> "noreferrer"
    | `opener -> "opener"
    | `pingback -> "pingback"
    | `preconnect -> "preconnect"
    | `prefetch -> "prefetch"
    | `preload -> "preload"
    | `prev -> "prev"
    | `search -> "search"
    | `stylesheet -> "stylesheet"
    | `tag -> "tag"
    | `other s -> s

  let string_of_target : target -> string = function
    | `blank -> "_blank"
    | `self -> "_self"
    | `parent -> "_parent"
    | `top -> "_top"
    | `frame s -> s

  let string_of_referrerpolicy : referrerpolicy -> string = function
    | `no_referrer -> "no-referrer"
    | `no_referrer_when_downgrade -> "no-referrer-when-downgrade"
    | `origin -> "origin"
    | `origin_when_cross_origin -> "origin-when-cross-origin"
    | `same_origin -> "same-origin"
    | `strict_origin -> "strict-origin"
    | `strict_origin_when_cross_origin -> "strict-origin-when-cross-origin"
    | `unsafe_url -> "unsafe-url"

  let string_of_crossorigin : crossorigin -> string = function
    | `anonymous -> "anonymous"
    | `use_credentials -> "use-credentials"

  let string_of_loading : loading -> string = function
    | `eager -> "eager"
    | `lazy_ -> "lazy"

  let string_of_form_method : form_method -> string = function
    | `get -> "get"
    | `post -> "post"
    | `dialog -> "dialog"

  let string_of_enctype : enctype -> string = function
    | `urlencoded -> "application/x-www-form-urlencoded"
    | `multipart -> "multipart/form-data"
    | `text_plain -> "text/plain"

  let string_of_dir : dir -> string = function
    | `ltr -> "ltr"
    | `rtl -> "rtl"
    | `auto -> "auto"

  let string_of_wrap : wrap -> string = function
    | `hard -> "hard"
    | `soft -> "soft"

  let string_of_scope : scope -> string = function
    | `row -> "row"
    | `col -> "col"
    | `rowgroup -> "rowgroup"
    | `colgroup -> "colgroup"

  let string_of_track_kind : track_kind -> string = function
    | `subtitles -> "subtitles"
    | `captions -> "captions"
    | `descriptions -> "descriptions"
    | `chapters -> "chapters"
    | `metadata -> "metadata"

  let string_of_preload : preload -> string = function
    | `none -> "none"
    | `metadata -> "metadata"
    | `auto -> "auto"

  let string_of_inputmode : inputmode -> string = function
    | `none -> "none"
    | `text -> "text"
    | `decimal -> "decimal"
    | `numeric -> "numeric"
    | `tel -> "tel"
    | `search -> "search"
    | `email -> "email"
    | `url -> "url"

  let string_of_enterkeyhint : enterkeyhint -> string = function
    | `enter -> "enter"
    | `done_ -> "done"
    | `go -> "go"
    | `next -> "next"
    | `previous -> "previous"
    | `search -> "search"
    | `send -> "send"

  let string_of_popover : popover -> string = function
    | `auto -> "auto"
    | `manual -> "manual"
    | `hint -> "hint"

  let string_of_autocomplete : autocomplete -> string = function
    | `on -> "on"
    | `off -> "off"
    | `name -> "name"
    | `given_name -> "given-name"
    | `family_name -> "family-name"
    | `email -> "email"
    | `username -> "username"
    | `new_password -> "new-password"
    | `current_password -> "current-password"
    | `one_time_code -> "one-time-code"
    | `tel -> "tel"
    | `url -> "url"
    | `street_address -> "street-address"
    | `postal_code -> "postal-code"
    | `country -> "country"
    | `organization -> "organization"
    | `bday -> "bday"
    | `other s -> s

  let string_of_http_equiv : http_equiv -> string = function
    | `content_security_policy -> "content-security-policy"
    | `content_type -> "content-type"
    | `default_style -> "default-style"
    | `refresh -> "refresh"
    | `x_ua_compatible -> "x-ua-compatible"

  let string_of_sandbox : sandbox -> string = function
    | `allow_downloads -> "allow-downloads"
    | `allow_forms -> "allow-forms"
    | `allow_modals -> "allow-modals"
    | `allow_orientation_lock -> "allow-orientation-lock"
    | `allow_pointer_lock -> "allow-pointer-lock"
    | `allow_popups -> "allow-popups"
    | `allow_popups_to_escape_sandbox -> "allow-popups-to-escape-sandbox"
    | `allow_presentation -> "allow-presentation"
    | `allow_same_origin -> "allow-same-origin"
    | `allow_scripts -> "allow-scripts"
    | `allow_top_navigation -> "allow-top-navigation"
    | `allow_top_navigation_by_user_activation -> "allow-top-navigation-by-user-activation"
    | `allow_top_navigation_to_custom_protocols -> "allow-top-navigation-to-custom-protocols"

  let string_of_input_type : input_type -> string = function
    | `button -> "button"
    | `checkbox -> "checkbox"
    | `color -> "color"
    | `date -> "date"
    | `datetime_local -> "datetime-local"
    | `email -> "email"
    | `file -> "file"
    | `hidden -> "hidden"
    | `image -> "image"
    | `month -> "month"
    | `number -> "number"
    | `password -> "password"
    | `radio -> "radio"
    | `range -> "range"
    | `reset -> "reset"
    | `search -> "search"
    | `submit -> "submit"
    | `tel -> "tel"
    | `text -> "text"
    | `time -> "time"
    | `url -> "url"
    | `week -> "week"

  let string_of_button_type : button_type -> string = function
    | `submit -> "submit"
    | `reset -> "reset"
    | `button -> "button"

  let string_of_script_type : script_type -> string = function
    | `module_ -> "module"
    | `importmap -> "importmap"
    | `speculationrules -> "speculationrules"
    | `mime s -> s

  let string_of_charset : charset -> string = function
    | `utf_8 -> "utf-8"

  let attr key value = (key, Some value)
  let flag key = (key, None)
  let data key value = ("data-" ^ key, Some value)
  let aria key value = ("aria-" ^ key, Some value)

  let id v = ("id", Some (Css.Id.name v))
  let class_ v = ("class", Some (Css.Class.name v))
  let classes vs = ("class", Some (String.concat " " (List.map Css.Class.name vs)))
  let style decls = ("style", Some (Css.inline decls))
  let title v = ("title", Some v)
  let lang v = ("lang", Some v)
  let dir v = ("dir", Some (string_of_dir v))
  let tabindex n = ("tabindex", Some (string_of_int n))
  let hidden = ("hidden", None)
  let inert = ("inert", None)
  let autofocus = ("autofocus", None)
  let role v = ("role", Some v)
  let accesskey v = ("accesskey", Some v)
  let contenteditable b = ("contenteditable", Some (if b then "true" else "false"))
  let draggable b = ("draggable", Some (if b then "true" else "false"))
  let spellcheck b = ("spellcheck", Some (if b then "true" else "false"))
  let translate b = ("translate", Some (if b then "yes" else "no"))
  let inputmode v = ("inputmode", Some (string_of_inputmode v))
  let enterkeyhint v = ("enterkeyhint", Some (string_of_enterkeyhint v))
  let popover v = ("popover", Some (string_of_popover v))
  let href v = ("href", Some v)
  let target v = ("target", Some (string_of_target v))
  let download = ("download", None)
  let download_as v = ("download", Some v)
  let rel v = ("rel", Some (string_of_rel v))
  let rels vs = ("rel", Some (String.concat " " (List.map string_of_rel vs)))
  let hreflang v = ("hreflang", Some v)
  let referrerpolicy v = ("referrerpolicy", Some (string_of_referrerpolicy v))
  let mime_type v = ("type", Some v)
  let src v = ("src", Some v)
  let srcset v = ("srcset", Some v)
  let sizes v = ("sizes", Some v)
  let alt v = ("alt", Some v)
  let width n = ("width", Some (string_of_int n))
  let height n = ("height", Some (string_of_int n))
  let loading v = ("loading", Some (string_of_loading v))
  let crossorigin v = ("crossorigin", Some (string_of_crossorigin v))
  let integrity v = ("integrity", Some v)
  let poster v = ("poster", Some v)
  let onload v = ("onload", Some v)
  let preload v = ("preload", Some (string_of_preload v))
  let controls = ("controls", None)
  let autoplay = ("autoplay", None)
  let loop = ("loop", None)
  let muted = ("muted", None)
  let playsinline = ("playsinline", None)
  let kind v = ("kind", Some (string_of_track_kind v))
  let srclang v = ("srclang", Some v)
  let label v = ("label", Some v)
  let default = ("default", None)
  let sandbox vs = ("sandbox", Some (String.concat " " (List.map string_of_sandbox vs)))
  let allow v = ("allow", Some v)
  let srcdoc v = ("srcdoc", Some v)
  let script_type v = ("type", Some (string_of_script_type v))
  let async = ("async", None)
  let defer = ("defer", None)
  let nomodule = ("nomodule", None)
  let charset v = ("charset", Some (string_of_charset v))
  let name v = ("name", Some v)
  let content v = ("content", Some v)
  let http_equiv v = ("http-equiv", Some (string_of_http_equiv v))
  let media v = ("media", Some v)
  let action v = ("action", Some v)
  let method_ v = ("method", Some (string_of_form_method v))
  let enctype v = ("enctype", Some (string_of_enctype v))
  let novalidate = ("novalidate", None)
  let accept_charset v = ("accept-charset", Some (string_of_charset v))
  let autocomplete v = ("autocomplete", Some (string_of_autocomplete v))
  let form v = ("form", Some v)
  let formaction v = ("formaction", Some v)
  let formmethod v = ("formmethod", Some (string_of_form_method v))
  let formnovalidate = ("formnovalidate", None)
  let value v = ("value", Some v)
  let disabled = ("disabled", None)
  let checked = ("checked", None)
  let selected = ("selected", None)
  let multiple = ("multiple", None)
  let readonly = ("readonly", None)
  let required = ("required", None)
  let input_type v = ("type", Some (string_of_input_type v))
  let button_type v = ("type", Some (string_of_button_type v))
  let accept vs = ("accept", Some (String.concat "," vs))
  let list v = ("list", Some v)
  let min v = ("min", Some v)
  let max v = ("max", Some v)
  let step v = ("step", Some v)
  let maxlength n = ("maxlength", Some (string_of_int n))
  let minlength n = ("minlength", Some (string_of_int n))
  let size n = ("size", Some (string_of_int n))
  let pattern v = ("pattern", Some v)
  let placeholder v = ("placeholder", Some v)
  let for_ v = ("for", Some v)
  let rows n = ("rows", Some (string_of_int n))
  let cols n = ("cols", Some (string_of_int n))
  let wrap v = ("wrap", Some (string_of_wrap v))
  let popovertarget v = ("popovertarget", Some v)
  let colspan n = ("colspan", Some (string_of_int n))
  let rowspan n = ("rowspan", Some (string_of_int n))
  let span n = ("span", Some (string_of_int n))
  let headers vs = ("headers", Some (String.concat " " vs))
  let scope v = ("scope", Some (string_of_scope v))
  let start n = ("start", Some (string_of_int n))
  let reversed = ("reversed", None)
  let cite v = ("cite", Some v)
  let datetime v = ("datetime", Some v)
  let open_ = ("open", None)
end


let html attributes children = tag "html" attributes children
let head attributes children = tag "head" attributes children
let body attributes children = tag "body" attributes children
let title attributes children = tag "title" attributes children
let base attributes = void_tag "base" attributes
let link attributes = void_tag "link" attributes
let meta attributes = void_tag "meta" attributes
let style attributes children = tag "style" attributes children
let script attributes children = tag "script" attributes children
let noscript attributes children = tag "noscript" attributes children
let template attributes children = tag "template" attributes children
let main attributes children = tag "main" attributes children
let header attributes children = tag "header" attributes children
let footer attributes children = tag "footer" attributes children
let nav attributes children = tag "nav" attributes children
let section attributes children = tag "section" attributes children
let article attributes children = tag "article" attributes children
let aside attributes children = tag "aside" attributes children
let address attributes children = tag "address" attributes children
let search attributes children = tag "search" attributes children
let h1 attributes children = tag "h1" attributes children
let h2 attributes children = tag "h2" attributes children
let h3 attributes children = tag "h3" attributes children
let h4 attributes children = tag "h4" attributes children
let h5 attributes children = tag "h5" attributes children
let h6 attributes children = tag "h6" attributes children
let hgroup attributes children = tag "hgroup" attributes children
let div attributes children = tag "div" attributes children
let p attributes children = tag "p" attributes children
let pre attributes children = tag "pre" attributes children
let blockquote attributes children = tag "blockquote" attributes children
let figure attributes children = tag "figure" attributes children
let figcaption attributes children = tag "figcaption" attributes children
let hr attributes = void_tag "hr" attributes
let ul attributes children = tag "ul" attributes children
let menu attributes children = tag "menu" attributes children
let ol attributes children = tag "ol" attributes children
let li attributes children = tag "li" attributes children
let dl attributes children = tag "dl" attributes children
let dt attributes children = tag "dt" attributes children
let dd attributes children = tag "dd" attributes children
let a attributes children = tag "a" attributes children
let span attributes children = tag "span" attributes children
let strong attributes children = tag "strong" attributes children
let em attributes children = tag "em" attributes children
let b attributes children = tag "b" attributes children
let i attributes children = tag "i" attributes children
let u attributes children = tag "u" attributes children
let s attributes children = tag "s" attributes children
let small attributes children = tag "small" attributes children
let mark attributes children = tag "mark" attributes children
let code attributes children = tag "code" attributes children
let kbd attributes children = tag "kbd" attributes children
let samp attributes children = tag "samp" attributes children
let var attributes children = tag "var" attributes children
let sub attributes children = tag "sub" attributes children
let sup attributes children = tag "sup" attributes children
let abbr attributes children = tag "abbr" attributes children
let bdi attributes children = tag "bdi" attributes children
let bdo attributes children = tag "bdo" attributes children
let cite attributes children = tag "cite" attributes children
let q attributes children = tag "q" attributes children
let time attributes children = tag "time" attributes children
let data attributes children = tag "data" attributes children
let del attributes children = tag "del" attributes children
let ins attributes children = tag "ins" attributes children
let ruby attributes children = tag "ruby" attributes children
let rt attributes children = tag "rt" attributes children
let rp attributes children = tag "rp" attributes children
let br attributes = void_tag "br" attributes
let wbr attributes = void_tag "wbr" attributes
let form attributes children = tag "form" attributes children
let label attributes children = tag "label" attributes children
let button attributes children = tag "button" attributes children
let input attributes = void_tag "input" attributes
let select attributes children = tag "select" attributes children
let optgroup attributes children = tag "optgroup" attributes children
let option attributes children = tag "option" attributes children
let textarea attributes children = tag "textarea" attributes children
let fieldset attributes children = tag "fieldset" attributes children
let legend attributes children = tag "legend" attributes children
let output attributes children = tag "output" attributes children
let datalist attributes children = tag "datalist" attributes children
let progress attributes children = tag "progress" attributes children
let meter attributes children = tag "meter" attributes children
let table attributes children = tag "table" attributes children
let caption attributes children = tag "caption" attributes children
let colgroup attributes children = tag "colgroup" attributes children
let col attributes = void_tag "col" attributes
let thead attributes children = tag "thead" attributes children
let tbody attributes children = tag "tbody" attributes children
let tfoot attributes children = tag "tfoot" attributes children
let tr attributes children = tag "tr" attributes children
let th attributes children = tag "th" attributes children
let td attributes children = tag "td" attributes children
let img attributes = void_tag "img" attributes
let picture attributes children = tag "picture" attributes children
let source attributes = void_tag "source" attributes
let video attributes children = tag "video" attributes children
let audio attributes children = tag "audio" attributes children
let track attributes = void_tag "track" attributes
let iframe attributes = tag "iframe" attributes []
let embed attributes = void_tag "embed" attributes
let canvas attributes children = tag "canvas" attributes children
let svg attributes children = tag "svg" attributes children
let map attributes children = tag "map" attributes children
let area attributes = void_tag "area" attributes
let details attributes children = tag "details" attributes children
let summary attributes children = tag "summary" attributes children
let dialog attributes children = tag "dialog" attributes children
let slot attributes children = tag "slot" attributes children
