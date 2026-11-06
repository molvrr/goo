(* Class and id names, shared between stylesheets and HTML. *)
module Class = struct
  type t = string
  let make name : t = name
  let name (t : t) = t
  let selector (t : t) = "." ^ t
end

module Id = struct
  type t = string
  let make name : t = name
  let name (t : t) = t
  let selector (t : t) = "#" ^ t
end

module Ast = struct
  (* Selectors *)

  type nth = [ `odd | `even | `n of int | `an_plus_b of int * int ]

  type attr_op =
    [ `exists                (* [a] *)
    | `equals of string      (* [a="v"] *)
    | `prefix of string      (* [a^="v"] *)
    | `suffix of string      (* [a$="v"] *)
    | `contains of string    (* [a*="v"] *)
    | `word of string        (* [a~="v"] *)
    | `dash of string ]      (* [a|="v"] *)

  (* [("id", `prefix "mail-")] is [[id^="mail-"]]. *)
  type attr_match = string * attr_op

  type pseudo_element =
    [ `before | `after | `placeholder | `selection | `marker | `first_line
    | `first_letter | `backdrop | `file_selector_button ]

  type pseudo_keyword =
    [ `hover | `focus | `focus_visible | `focus_within | `active | `visited
    | `link | `any_link | `checked | `disabled | `enabled | `required | `optional
    | `valid | `invalid | `read_only | `read_write | `placeholder_shown | `empty
    | `first_child | `last_child | `only_child | `first_of_type | `last_of_type
    | `only_of_type | `root | `target | `defined | `open_ | `popover_open ]

  (* ['s] is the type of selector arguments ([:not(…)] and friends). *)
  type 's pseudo =
    [ pseudo_keyword
    | `nth_child of nth | `nth_last_child of nth | `nth_of_type of nth | `nth_last_of_type of nth
    | `not of 's list | `is of 's list | `where of 's list | `has of 's list | `lang of string ]

  type combinator = Descendant | Child | Next_sibling | Subsequent_sibling

  type compound = {
    tag : string option;
    id : string option;
    classes : string list;
    attrs : attr_match list;
    pseudos : selector pseudo list;
    pseudo_element : pseudo_element option;
  }

  (* A complex selector: compounds joined by combinators, left to right. *)
  and selector = compound * (combinator * compound) list

  type length_unit = Px | Em | Rem | Vh | Vw | Vmin | Vmax | Ch | Pt | Fr | Deg | Ms | S

  type color =
    | Hex of string                                (* without the leading # *)
    | Rgb of int * int * int * float option
    | Hsl of int * int * int * float option
    | Hwb of float * float * float * float option
    | Lab of float * float * float * float option     (** lightness in % *)
    | Lch of float * float * float * float option     (** lightness in % *)
    | Oklab of float * float * float * float option   (** lightness 0–1 *)
    | Oklch of float * float * float * float option   (** lightness 0–1 *)

  type value =
    | Keyword of string
    | Integer of int
    | Number of float
    | Length of float * length_unit
    | Percentage of float
    | Color of color
    | String of string
    | Url of string
    | Var of string * value option
    | Function of string * value list
    | Space of value list
    | Comma of value list
    | Raw of string

  type declaration = { property : string; value : value; important : bool }

  type keyframe_selector = From | To | At of float

  type stmt =
    | Rule of selector list * declaration list
    | Media of string * stmt list
    | Supports of string * stmt list
    | Font_face of declaration list
    | Keyframes of string * (keyframe_selector list * declaration list) list
    | Import of string * string option
    | Raw_css of string

  type stylesheet = stmt list
end

module Selector = struct
  open Ast

  type nonrec nth = nth
  type nonrec attr_op = attr_op
  type nonrec attr_match = attr_match
  type nonrec pseudo_element = pseudo_element

  (* A selector is a function from declarations to a rule, so it can be
     applied directly: [(any > ul !li) [color `red]]. Its selectors are
     recovered by applying it to the empty list. *)
  type t = declaration list -> stmt

  type pseudo_class = t pseudo

  type builder =
    ?id:Id.t -> ?cls:Class.t list -> ?attr:attr_match list ->
    ?pseudo:pseudo_class list -> ?pseudo_el:pseudo_element -> t -> t

  let empty_compound =
    { tag = None; id = None; classes = []; attrs = []; pseudos = []; pseudo_element = None }
  let nil_selector : selector = (empty_compound, [])
  let is_nil_selector (c, rest) = c = empty_compound && rest = []

  let of_selectors (sels : selector list) : t =
    let sels = List.filter (fun s -> not (is_nil_selector s)) sels in
    fun declarations -> Rule (sels, declarations)

  let selectors (s : t) =
    match s [] with Rule (sels, _) -> sels | _ -> invalid_arg "Css.Selector.selectors"

  let nil : t = fun declarations -> Rule ([nil_selector], declarations)
  let is_nil s = match s [] with Rule ([x], _) -> is_nil_selector x | _ -> false

  let join_selector comb ((x, xs) as a) ((y, ys) as b) : selector =
    if is_nil_selector a then b
    else if is_nil_selector b then a
    else (x, xs @ ((comb, y) :: ys))

  (* Groups distribute: [(a, b) > c] is [a > c, b > c]. *)
  let join comb (a : t) (b : t) : t =
    let joined =
      List.concat_map (fun x -> List.map (fun y -> join_selector comb x y) (selectors b)) (selectors a) in
    fun declarations -> Rule (joined, declarations)

  let group (ss : t list) : t = of_selectors (List.concat_map selectors ss)

  let convert_pseudo : pseudo_class -> selector pseudo = function
    | #pseudo_keyword as k -> k
    | `nth_child n -> `nth_child n
    | `nth_last_child n -> `nth_last_child n
    | `nth_of_type n -> `nth_of_type n
    | `nth_last_of_type n -> `nth_last_of_type n
    | `lang l -> `lang l
    | `not ts -> `not (List.concat_map selectors ts)
    | `is ts -> `is (List.concat_map selectors ts)
    | `where ts -> `where (List.concat_map selectors ts)
    | `has ts -> `has (List.concat_map selectors ts)

  let el ?tag ?id ?(cls = []) ?(attr = []) ?(pseudo = []) ?pseudo_el next =
    let here =
      { tag; id = Option.map Id.name id; classes = List.map Class.name cls; attrs = attr;
        pseudos = List.map convert_pseudo pseudo; pseudo_element = pseudo_el } in
    join Descendant (of_selectors [(here, [])]) next

  let tag name : builder =
    fun ?id ?cls ?attr ?pseudo ?pseudo_el next -> el ~tag:name ?id ?cls ?attr ?pseudo ?pseudo_el next

  let cls (c : Class.t) : builder =
    fun ?id ?(cls = []) ?attr ?pseudo ?pseudo_el next ->
      el ?id ~cls:(c :: cls) ?attr ?pseudo ?pseudo_el next

  let id (i : Id.t) ?cls ?attr ?pseudo ?pseudo_el next = el ~id:i ?cls ?attr ?pseudo ?pseudo_el next

  let any : t = of_selectors [({ empty_compound with tag = Some "*" }, [])]
  let root : t = of_selectors [({ empty_compound with pseudos = [`root] }, [])]

  let html = tag "html"
  let head = tag "head"
  let body = tag "body"
  let title = tag "title"
  let style = tag "style"
  let script = tag "script"
  let noscript = tag "noscript"
  let template = tag "template"
  let main = tag "main"
  let header = tag "header"
  let footer = tag "footer"
  let nav = tag "nav"
  let section = tag "section"
  let article = tag "article"
  let aside = tag "aside"
  let address = tag "address"
  let search = tag "search"
  let h1 = tag "h1"
  let h2 = tag "h2"
  let h3 = tag "h3"
  let h4 = tag "h4"
  let h5 = tag "h5"
  let h6 = tag "h6"
  let hgroup = tag "hgroup"
  let div = tag "div"
  let p = tag "p"
  let pre = tag "pre"
  let blockquote = tag "blockquote"
  let figure = tag "figure"
  let figcaption = tag "figcaption"
  let hr = tag "hr"
  let ul = tag "ul"
  let menu = tag "menu"
  let ol = tag "ol"
  let li = tag "li"
  let dl = tag "dl"
  let dt = tag "dt"
  let dd = tag "dd"
  let a = tag "a"
  let span = tag "span"
  let strong = tag "strong"
  let em = tag "em"
  let b = tag "b"
  let i = tag "i"
  let u = tag "u"
  let s = tag "s"
  let small = tag "small"
  let mark = tag "mark"
  let code = tag "code"
  let kbd = tag "kbd"
  let samp = tag "samp"
  let var = tag "var"
  let sub = tag "sub"
  let sup = tag "sup"
  let abbr = tag "abbr"
  let bdi = tag "bdi"
  let bdo = tag "bdo"
  let cite = tag "cite"
  let q = tag "q"
  let time = tag "time"
  let data = tag "data"
  let del = tag "del"
  let ins = tag "ins"
  let ruby = tag "ruby"
  let rt = tag "rt"
  let rp = tag "rp"
  let br = tag "br"
  let wbr = tag "wbr"
  let form = tag "form"
  let label = tag "label"
  let button = tag "button"
  let input = tag "input"
  let select = tag "select"
  let optgroup = tag "optgroup"
  let option = tag "option"
  let textarea = tag "textarea"
  let fieldset = tag "fieldset"
  let legend = tag "legend"
  let output = tag "output"
  let datalist = tag "datalist"
  let progress = tag "progress"
  let meter = tag "meter"
  let table = tag "table"
  let caption = tag "caption"
  let colgroup = tag "colgroup"
  let col = tag "col"
  let thead = tag "thead"
  let tbody = tag "tbody"
  let tfoot = tag "tfoot"
  let tr = tag "tr"
  let th = tag "th"
  let td = tag "td"
  let img = tag "img"
  let picture = tag "picture"
  let source = tag "source"
  let video = tag "video"
  let audio = tag "audio"
  let track = tag "track"
  let iframe = tag "iframe"
  let embed = tag "embed"
  let canvas = tag "canvas"
  let svg = tag "svg"
  let map = tag "map"
  let area = tag "area"
  let details = tag "details"
  let summary = tag "summary"
  let dialog = tag "dialog"
  let slot = tag "slot"

  let descendant = join Descendant
  let child = join Child
  let next_sibling = join Next_sibling
  let subsequent_sibling = join Subsequent_sibling

  (* Printing *)

  let string_of_pseudo_element : pseudo_element -> string = function
    | `before -> "before"
    | `after -> "after"
    | `placeholder -> "placeholder"
    | `selection -> "selection"
    | `marker -> "marker"
    | `first_line -> "first-line"
    | `first_letter -> "first-letter"
    | `backdrop -> "backdrop"
    | `file_selector_button -> "file-selector-button"

  let string_of_nth : nth -> string = function
    | `odd -> "odd"
    | `even -> "even"
    | `n n -> string_of_int n
    | `an_plus_b (a, 0) -> string_of_int a ^ "n"
    | `an_plus_b (a, b) -> Printf.sprintf "%dn%+d" a b

  let quote s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\a "
      | c -> Buffer.add_char buf c) s;
    Buffer.add_char buf '"';
    Buffer.contents buf

  let string_of_attr ((a, op) : attr_match) =
    let with_ sym v = "[" ^ a ^ sym ^ quote v ^ "]" in
    match op with
    | `exists -> "[" ^ a ^ "]"
    | `equals v -> with_ "=" v
    | `prefix v -> with_ "^=" v
    | `suffix v -> with_ "$=" v
    | `contains v -> with_ "*=" v
    | `word v -> with_ "~=" v
    | `dash v -> with_ "|=" v

  let rec print ?(minify = false) ((first, rest) : selector) =
    let comb = function
      | Descendant -> " "
      | Child -> if minify then ">" else " > "
      | Next_sibling -> if minify then "+" else " + "
      | Subsequent_sibling -> if minify then "~" else " ~ " in
    String.concat ""
      (print_compound ~minify first
       :: List.concat_map (fun (c, x) -> [comb c; print_compound ~minify x]) rest)

  and print_list ?(minify = false) sels =
    String.concat (if minify then "," else ", ") (List.map (print ~minify) sels)

  and string_of_pseudo_class ~minify : selector pseudo -> string = function
    | `hover -> "hover"
    | `focus -> "focus"
    | `focus_visible -> "focus-visible"
    | `focus_within -> "focus-within"
    | `active -> "active"
    | `visited -> "visited"
    | `link -> "link"
    | `any_link -> "any-link"
    | `checked -> "checked"
    | `disabled -> "disabled"
    | `enabled -> "enabled"
    | `required -> "required"
    | `optional -> "optional"
    | `valid -> "valid"
    | `invalid -> "invalid"
    | `read_only -> "read-only"
    | `read_write -> "read-write"
    | `placeholder_shown -> "placeholder-shown"
    | `empty -> "empty"
    | `first_child -> "first-child"
    | `last_child -> "last-child"
    | `only_child -> "only-child"
    | `first_of_type -> "first-of-type"
    | `last_of_type -> "last-of-type"
    | `only_of_type -> "only-of-type"
    | `root -> "root"
    | `target -> "target"
    | `defined -> "defined"
    | `open_ -> "open"
    | `popover_open -> "popover-open"
    | `nth_child n -> "nth-child(" ^ string_of_nth n ^ ")"
    | `nth_last_child n -> "nth-last-child(" ^ string_of_nth n ^ ")"
    | `nth_of_type n -> "nth-of-type(" ^ string_of_nth n ^ ")"
    | `nth_last_of_type n -> "nth-last-of-type(" ^ string_of_nth n ^ ")"
    | `not ts -> "not(" ^ print_list ~minify ts ^ ")"
    | `is ts -> "is(" ^ print_list ~minify ts ^ ")"
    | `where ts -> "where(" ^ print_list ~minify ts ^ ")"
    | `has ts -> "has(" ^ print_list ~minify ts ^ ")"
    | `lang l -> "lang(" ^ l ^ ")"

  and print_compound ~minify c =
    if c = empty_compound then "*"
    else
      String.concat ""
        (Option.to_list c.tag
         @ List.map (fun i -> "#" ^ i) (Option.to_list c.id)
         @ List.map (fun k -> "." ^ k) c.classes
         @ List.map string_of_attr c.attrs
         @ List.map (fun p -> ":" ^ string_of_pseudo_class ~minify p) c.pseudos
         @ List.map (fun p -> "::" ^ string_of_pseudo_element p) (Option.to_list c.pseudo_element))

  let to_string ?(minify = false) s = print_list ~minify (selectors s)

  (* Specificity *)

  let add3 (a, b, c) (d, e, f) = (a + d, b + e, c + f)
  let max3 l = List.fold_left max (0, 0, 0) l

  let rec selector_specificity ((first, rest) : selector) =
    List.fold_left (fun acc (_, c) -> add3 acc (compound_specificity c))
      (compound_specificity first) rest

  and compound_specificity c =
    let ids = if c.id = None then 0 else 1 in
    let tags = match c.tag with None | Some "*" -> 0 | Some _ -> 1 in
    let pe = if c.pseudo_element = None then 0 else 1 in
    let base = (ids, List.length c.classes + List.length c.attrs, tags + pe) in
    List.fold_left (fun acc p ->
      add3 acc (match p with
        | `where _ -> (0, 0, 0)
        | `not ts | `is ts | `has ts -> max3 (List.map selector_specificity ts)
        | _ -> (0, 1, 0))) base c.pseudos

  (* Highest specificity among the group. *)
  let specificity s = max3 (List.map selector_specificity (selectors s))

  (* Operators last: they shadow Stdlib inside this module. *)
  let ( ! ) (f : t -> t) = f nil
  let ( > ) = child
  let ( + ) = next_sibling
  let ( +~ ) = subsequent_sibling
end

open Ast

type declaration = Ast.declaration
type stmt = Ast.stmt
type stylesheet = Ast.stylesheet

(* ---------- Typed values ---------- *)

  type global_kw =
    [ `inherit_ | `initial | `unset | `revert ]

  type named_color =
    [ `black | `white | `red | `green | `blue | `yellow | `orange | `purple
    | `pink | `gray | `grey | `silver | `maroon | `navy | `teal | `olive | `lime
    | `aqua | `fuchsia | `brown | `cyan | `magenta | `gold | `indigo | `violet
    | `transparent | `current_color ]

  type display =
    [ `block | `inline | `inline_block | `flex | `inline_flex | `grid | `inline_grid
    | `none | `contents | `flow_root | `list_item | `table | `table_row | `table_cell ]

  type position =
    [ `static | `relative | `absolute | `fixed | `sticky ]

  type box_sizing =
    [ `border_box | `content_box ]

  type overflow =
    [ `visible | `hidden | `clip | `scroll | `auto ]

  type flex_direction =
    [ `row | `row_reverse | `column | `column_reverse ]

  type flex_wrap =
    [ `nowrap | `wrap | `wrap_reverse ]

  type alignment =
    [ `normal | `stretch | `center | `start | `end_ | `flex_start | `flex_end
    | `space_between | `space_around | `space_evenly | `baseline | `auto ]

  type font_style =
    [ `normal | `italic | `oblique ]

  type font_size_kw =
    [ `xx_small | `x_small | `small | `medium | `large | `x_large | `xx_large
    | `smaller | `larger ]

  type text_align =
    [ `left | `right | `center | `justify | `start | `end_ ]

  type text_decoration =
    [ `none | `underline | `overline | `line_through ]

  type text_transform =
    [ `none | `uppercase | `lowercase | `capitalize ]

  type white_space =
    [ `normal | `nowrap | `pre | `pre_wrap | `pre_line | `break_spaces ]

  type text_overflow =
    [ `clip | `ellipsis ]

  type word_break =
    [ `normal | `break_all | `keep_all | `break_word ]

  type overflow_wrap =
    [ `normal | `break_word | `anywhere ]

  type list_style_type =
    [ `none | `disc | `circle | `square | `decimal ]

  type list_style_position =
    [ `inside | `outside ]

  type cursor =
    [ `auto | `default | `pointer | `text | `move | `not_allowed | `grab | `grabbing
    | `wait | `progress | `help | `crosshair | `none | `col_resize | `row_resize ]

  type pointer_events =
    [ `auto | `none ]

  type user_select =
    [ `auto | `none | `text | `all ]

  type visibility =
    [ `visible | `hidden | `collapse ]

  type border_style =
    [ `none | `hidden | `solid | `dashed | `dotted | `double | `groove | `ridge
    | `inset | `outset ]

  type border_width_kw =
    [ `thin | `medium | `thick ]

  type border_collapse =
    [ `collapse | `separate ]

  type background_repeat =
    [ `repeat | `no_repeat | `repeat_x | `repeat_y | `space | `round ]

  type background_clip = [ `border_box | `padding_box | `content_box | `text ]

  type background_attachment = [ `scroll | `fixed | `local ]

  type font_variant_numeric =
    [ `normal | `tabular_nums | `proportional_nums | `lining_nums | `oldstyle_nums
    | `slashed_zero | `ordinal | `diagonal_fractions | `stacked_fractions ]

  type background_size_kw =
    [ `cover | `contain | `auto ]

  type object_fit =
    [ `fill | `contain | `cover | `none | `scale_down ]

  type vertical_align =
    [ `baseline | `top | `middle | `bottom | `text_top | `text_bottom | `sub
    | `super ]

  type resize =
    [ `none | `both | `horizontal | `vertical ]

  type scroll_behavior =
    [ `auto | `smooth ]

  type float_kw =
    [ `left | `right | `none ]

  type clear =
    [ `left | `right | `both | `none ]

  type table_layout =
    [ `auto | `fixed ]

  type font_display =
    [ `auto | `block | `swap | `fallback | `optional ]

  type generic_family =
    [ `serif | `sans_serif | `monospace | `cursive | `fantasy | `system_ui
    | `ui_serif | `ui_sans_serif | `ui_monospace ]

  type font_weight_kw =
    [ `normal | `bold | `bolder | `lighter ]

  type global = [ global_kw | `var of string | `raw of string ]

  type length =
    [ `zero | `px of int | `em of float | `rem of float | `percent of float
    | `vh of float | `vw of float | `vmin of float | `vmax of float
    | `ch of float | `pt of float | `fr of float | `calc of string | `var of string ]

  type color_space =
    [ `srgb | `srgb_linear | `display_p3 | `oklab | `oklch | `lab | `lch | `hsl | `hwb | `xyz ]

  type relative_space = [ `rgb | `hsl | `hwb | `lab | `lch | `oklab | `oklch ]

  type color =
    [ named_color | `hex of string | `rgb of int * int * int
    | `rgba of int * int * int * float | `hsl of int * int * int
    | `hsla of int * int * int * float | `hwb of float * float * float
    | `lab of float * float * float | `laba of float * float * float * float
    | `lch of float * float * float | `lcha of float * float * float * float
    | `oklab of float * float * float | `oklaba of float * float * float * float
    | `oklch of float * float * float | `oklcha of float * float * float * float
    | `named of string | `var of string
    | `mix of color_space * color * float * color
    | `from of relative_space * color * string * string * string * string option ]

  type font_weight = [ font_weight_kw | `w of int ]
  type font_family = [ generic_family | `name of string ]
  type number = [ `num of float ]

  (** Every non-nested value. *)
  type time = [ `ms of float | `s of float ]

  type easing =
    [ `linear | `ease | `ease_in | `ease_out | `ease_in_out | `step_start | `step_end
    | `cubic_bezier of float * float * float * float | `steps of int ]

  type transform_fn =
    [ `translate of length * length | `translate_x of length | `translate_y of length
    | `scale of float | `scale_xy of float * float | `scale_x of float | `scale_y of float
    | `rotate of float                                        (* degrees *)
    | `skew_x of float | `skew_y of float                    (* degrees *)
    | `perspective of length ]

  type filter_fn =
    [ `blur of length | `brightness of float | `contrast of float | `grayscale of float
    | `hue_rotate of float                                    (* degrees *)
    | `invert of float | `opacity of float | `saturate of float | `sepia of float
    | `drop_shadow of length * length * length * color ]

  type track_size = [ length | `auto | `min_content | `max_content ]

  type track =
    [ length | `auto | `min_content | `max_content
    | `minmax of track_size * track_size
    | `fit_content of length
    | `repeat of [ `n of int | `auto_fill | `auto_fit ] * track list ]

  type grid_line =
    [ `auto | `line of int | `span of int | `between of int * int | `area of string ]

  type position_xy = [ length | `left | `center | `right | `top | `bottom ]

  type clip_shape =
    [ `none
    | `inset of length * length * length * length
    | `circle of length * (length * length)                   (* radius, at (x, y) *)
    | `polygon of (length * length) list
    | `url of string ]

  (* A color stop: a color and an optional position. *)
  type stop = color * length option

  type radial_shape = [ `circle | `ellipse | `size of length * length ]

  type image =
    [ `url of string
    | `linear of float * stop list                              (* angle in degrees *)
    | `radial of radial_shape * (length * length) * stop list ]  (* shape, at (x, y) *)

  type shadow =
    [ `shadow of length * length * length * color                  (* x y blur color *)
    | `spread of length * length * length * length * color         (* x y blur spread color *)
    | `inner of length * length * length * length * color ]        (* inset x y blur spread color *)

  type simple =
    [ `content | `padding_box | `local | `tabular_nums | `proportional_nums | `lining_nums | `oldstyle_nums | `slashed_zero | `ordinal | `diagonal_fractions | `stacked_fractions
    | `inherit_ | `initial | `unset | `revert
    | `block | `inline | `inline_block | `flex | `inline_flex | `grid | `inline_grid
    | `none | `contents | `flow_root | `list_item | `table | `table_row | `table_cell
    | `static | `relative | `absolute | `fixed | `sticky | `border_box | `content_box
    | `visible | `hidden | `clip | `scroll | `auto | `row | `row_reverse | `column
    | `column_reverse | `nowrap | `wrap | `wrap_reverse | `normal | `stretch
    | `center | `start | `end_ | `flex_start | `flex_end | `space_between
    | `space_around | `space_evenly | `baseline | `italic | `oblique | `xx_small
    | `x_small | `small | `medium | `large | `x_large | `xx_large | `smaller
    | `larger | `left | `right | `justify | `underline | `overline | `line_through
    | `uppercase | `lowercase | `capitalize | `pre | `pre_wrap | `pre_line
    | `break_spaces | `ellipsis | `break_all | `keep_all | `break_word | `anywhere
    | `disc | `circle | `square | `decimal | `inside | `outside | `default
    | `pointer | `text | `move | `not_allowed | `grab | `grabbing | `wait
    | `progress | `help | `crosshair | `col_resize | `row_resize | `all | `collapse
    | `solid | `dashed | `dotted | `double | `groove | `ridge | `inset | `outset
    | `thin | `thick | `separate | `repeat | `no_repeat | `repeat_x | `repeat_y
    | `space | `round | `cover | `contain | `fill | `scale_down | `top | `middle
    | `bottom | `text_top | `text_bottom | `sub | `super | `both | `horizontal
    | `vertical | `smooth | `swap | `fallback | `optional | `serif | `sans_serif
    | `monospace | `cursive | `fantasy | `system_ui | `ui_serif | `ui_sans_serif
    | `ui_monospace | `bold | `bolder | `lighter
    | `var of string | `raw of string | length | color
    | `w of int | `num of float | `int of int | `name of string | `url of string
    | `string of string | `attr of string | `ratio of int * int ]

  let string_of_color_space : color_space -> string = function
    | `srgb -> "srgb" | `srgb_linear -> "srgb-linear" | `display_p3 -> "display-p3"
    | `oklab -> "oklab" | `oklch -> "oklch" | `lab -> "lab" | `lch -> "lch"
    | `hsl -> "hsl" | `hwb -> "hwb" | `xyz -> "xyz"

  let string_of_relative_space : relative_space -> string = function
    | `rgb -> "rgb" | `hsl -> "hsl" | `hwb -> "hwb" | `lab -> "lab" | `lch -> "lch"
    | `oklab -> "oklab" | `oklch -> "oklch"

  let rec color_value : color -> value = function
    | `black -> Keyword "black"
    | `white -> Keyword "white"
    | `red -> Keyword "red"
    | `green -> Keyword "green"
    | `blue -> Keyword "blue"
    | `yellow -> Keyword "yellow"
    | `orange -> Keyword "orange"
    | `purple -> Keyword "purple"
    | `pink -> Keyword "pink"
    | `gray -> Keyword "gray"
    | `grey -> Keyword "grey"
    | `silver -> Keyword "silver"
    | `maroon -> Keyword "maroon"
    | `navy -> Keyword "navy"
    | `teal -> Keyword "teal"
    | `olive -> Keyword "olive"
    | `lime -> Keyword "lime"
    | `aqua -> Keyword "aqua"
    | `fuchsia -> Keyword "fuchsia"
    | `brown -> Keyword "brown"
    | `cyan -> Keyword "cyan"
    | `magenta -> Keyword "magenta"
    | `gold -> Keyword "gold"
    | `indigo -> Keyword "indigo"
    | `violet -> Keyword "violet"
    | `transparent -> Keyword "transparent"
    | `current_color -> Keyword "currentcolor"
    | `hex s -> Color (Hex (if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1) else s))
    | `rgb (r, g, b) -> Color (Rgb (r, g, b, None))
    | `rgba (r, g, b, a) -> Color (Rgb (r, g, b, Some a))
    | `hsl (h, s, l) -> Color (Hsl (h, s, l, None))
    | `hsla (h, s, l, a) -> Color (Hsl (h, s, l, Some a))
    | `hwb (h, w, b) -> Color (Hwb (h, w, b, None))
    | `lab (l, a, b) -> Color (Lab (l, a, b, None))
    | `laba (l, a, b, alpha) -> Color (Lab (l, a, b, Some alpha))
    | `lch (l, c, h) -> Color (Lch (l, c, h, None))
    | `lcha (l, c, h, a) -> Color (Lch (l, c, h, Some a))
    | `oklab (l, a, b) -> Color (Oklab (l, a, b, None))
    | `oklaba (l, a, b, alpha) -> Color (Oklab (l, a, b, Some alpha))
    | `oklch (l, c, h) -> Color (Oklch (l, c, h, None))
    | `oklcha (l, c, h, a) -> Color (Oklch (l, c, h, Some a))
    | `named s -> Keyword s
    | `var name -> Var (name, None)
    | `mix (space, a, p, b) ->
        Function ("color-mix",
          [Raw ("in " ^ string_of_color_space space); Space [color_value a; Percentage p]; color_value b])
    | `from (space, base, x, y, z, alpha) ->
        Function (string_of_relative_space space,
          [Space ([Keyword "from"; color_value base; Raw x; Raw y; Raw z]
                  @ (match alpha with None -> [] | Some a -> [Keyword "/"; Raw a]))])

  let value_of : [< simple ] -> value = function
    | `content -> Keyword "content"
    | `padding_box -> Keyword "padding-box"
    | `local -> Keyword "local"
    | `tabular_nums -> Keyword "tabular-nums"
    | `proportional_nums -> Keyword "proportional-nums"
    | `lining_nums -> Keyword "lining-nums"
    | `oldstyle_nums -> Keyword "oldstyle-nums"
    | `slashed_zero -> Keyword "slashed-zero"
    | `ordinal -> Keyword "ordinal"
    | `diagonal_fractions -> Keyword "diagonal-fractions"
    | `stacked_fractions -> Keyword "stacked-fractions"
    | `inherit_ -> Keyword "inherit"
    | `initial -> Keyword "initial"
    | `unset -> Keyword "unset"
    | `revert -> Keyword "revert"
    | `block -> Keyword "block"
    | `inline -> Keyword "inline"
    | `inline_block -> Keyword "inline-block"
    | `flex -> Keyword "flex"
    | `inline_flex -> Keyword "inline-flex"
    | `grid -> Keyword "grid"
    | `inline_grid -> Keyword "inline-grid"
    | `none -> Keyword "none"
    | `contents -> Keyword "contents"
    | `flow_root -> Keyword "flow-root"
    | `list_item -> Keyword "list-item"
    | `table -> Keyword "table"
    | `table_row -> Keyword "table-row"
    | `table_cell -> Keyword "table-cell"
    | `static -> Keyword "static"
    | `relative -> Keyword "relative"
    | `absolute -> Keyword "absolute"
    | `fixed -> Keyword "fixed"
    | `sticky -> Keyword "sticky"
    | `border_box -> Keyword "border-box"
    | `content_box -> Keyword "content-box"
    | `visible -> Keyword "visible"
    | `hidden -> Keyword "hidden"
    | `clip -> Keyword "clip"
    | `scroll -> Keyword "scroll"
    | `auto -> Keyword "auto"
    | `row -> Keyword "row"
    | `row_reverse -> Keyword "row-reverse"
    | `column -> Keyword "column"
    | `column_reverse -> Keyword "column-reverse"
    | `nowrap -> Keyword "nowrap"
    | `wrap -> Keyword "wrap"
    | `wrap_reverse -> Keyword "wrap-reverse"
    | `normal -> Keyword "normal"
    | `stretch -> Keyword "stretch"
    | `center -> Keyword "center"
    | `start -> Keyword "start"
    | `end_ -> Keyword "end"
    | `flex_start -> Keyword "flex-start"
    | `flex_end -> Keyword "flex-end"
    | `space_between -> Keyword "space-between"
    | `space_around -> Keyword "space-around"
    | `space_evenly -> Keyword "space-evenly"
    | `baseline -> Keyword "baseline"
    | `italic -> Keyword "italic"
    | `oblique -> Keyword "oblique"
    | `xx_small -> Keyword "xx-small"
    | `x_small -> Keyword "x-small"
    | `small -> Keyword "small"
    | `medium -> Keyword "medium"
    | `large -> Keyword "large"
    | `x_large -> Keyword "x-large"
    | `xx_large -> Keyword "xx-large"
    | `smaller -> Keyword "smaller"
    | `larger -> Keyword "larger"
    | `left -> Keyword "left"
    | `right -> Keyword "right"
    | `justify -> Keyword "justify"
    | `underline -> Keyword "underline"
    | `overline -> Keyword "overline"
    | `line_through -> Keyword "line-through"
    | `uppercase -> Keyword "uppercase"
    | `lowercase -> Keyword "lowercase"
    | `capitalize -> Keyword "capitalize"
    | `pre -> Keyword "pre"
    | `pre_wrap -> Keyword "pre-wrap"
    | `pre_line -> Keyword "pre-line"
    | `break_spaces -> Keyword "break-spaces"
    | `ellipsis -> Keyword "ellipsis"
    | `break_all -> Keyword "break-all"
    | `keep_all -> Keyword "keep-all"
    | `break_word -> Keyword "break-word"
    | `anywhere -> Keyword "anywhere"
    | `disc -> Keyword "disc"
    | `circle -> Keyword "circle"
    | `square -> Keyword "square"
    | `decimal -> Keyword "decimal"
    | `inside -> Keyword "inside"
    | `outside -> Keyword "outside"
    | `default -> Keyword "default"
    | `pointer -> Keyword "pointer"
    | `text -> Keyword "text"
    | `move -> Keyword "move"
    | `not_allowed -> Keyword "not-allowed"
    | `grab -> Keyword "grab"
    | `grabbing -> Keyword "grabbing"
    | `wait -> Keyword "wait"
    | `progress -> Keyword "progress"
    | `help -> Keyword "help"
    | `crosshair -> Keyword "crosshair"
    | `col_resize -> Keyword "col-resize"
    | `row_resize -> Keyword "row-resize"
    | `all -> Keyword "all"
    | `collapse -> Keyword "collapse"
    | `solid -> Keyword "solid"
    | `dashed -> Keyword "dashed"
    | `dotted -> Keyword "dotted"
    | `double -> Keyword "double"
    | `groove -> Keyword "groove"
    | `ridge -> Keyword "ridge"
    | `inset -> Keyword "inset"
    | `outset -> Keyword "outset"
    | `thin -> Keyword "thin"
    | `thick -> Keyword "thick"
    | `separate -> Keyword "separate"
    | `repeat -> Keyword "repeat"
    | `no_repeat -> Keyword "no-repeat"
    | `repeat_x -> Keyword "repeat-x"
    | `repeat_y -> Keyword "repeat-y"
    | `space -> Keyword "space"
    | `round -> Keyword "round"
    | `cover -> Keyword "cover"
    | `contain -> Keyword "contain"
    | `fill -> Keyword "fill"
    | `scale_down -> Keyword "scale-down"
    | `top -> Keyword "top"
    | `middle -> Keyword "middle"
    | `bottom -> Keyword "bottom"
    | `text_top -> Keyword "text-top"
    | `text_bottom -> Keyword "text-bottom"
    | `sub -> Keyword "sub"
    | `super -> Keyword "super"
    | `both -> Keyword "both"
    | `horizontal -> Keyword "horizontal"
    | `vertical -> Keyword "vertical"
    | `smooth -> Keyword "smooth"
    | `swap -> Keyword "swap"
    | `fallback -> Keyword "fallback"
    | `optional -> Keyword "optional"
    | `serif -> Keyword "serif"
    | `sans_serif -> Keyword "sans-serif"
    | `monospace -> Keyword "monospace"
    | `cursive -> Keyword "cursive"
    | `fantasy -> Keyword "fantasy"
    | `system_ui -> Keyword "system-ui"
    | `ui_serif -> Keyword "ui-serif"
    | `ui_sans_serif -> Keyword "ui-sans-serif"
    | `ui_monospace -> Keyword "ui-monospace"
    | `bold -> Keyword "bold"
    | `bolder -> Keyword "bolder"
    | `lighter -> Keyword "lighter"
    | `var name -> Var (name, None)
    | `raw s -> Raw s
    | `zero -> Integer 0
    | `px n -> Length (float_of_int n, Px)
    | `em f -> Length (f, Em)
    | `rem f -> Length (f, Rem)
    | `percent f -> Percentage f
    | `vh f -> Length (f, Vh)
    | `vw f -> Length (f, Vw)
    | `vmin f -> Length (f, Vmin)
    | `vmax f -> Length (f, Vmax)
    | `ch f -> Length (f, Ch)
    | `pt f -> Length (f, Pt)
    | `fr f -> Length (f, Fr)
    | `calc s -> Function ("calc", [Raw s])
    | #color as c -> color_value c
    | `w n -> Integer n
    | `num f -> Number f
    | `int n -> Integer n
    | `name s -> String s
    | `url s -> Url s
    | `string s -> String s
    | `attr a -> Function ("attr", [Keyword a])
    | `ratio (a, b) -> Space [Integer a; Keyword "/"; Integer b]

let border_value = function
  | `border (w, s, c) -> Space [value_of w; value_of s; value_of c]
  | #simple as v -> value_of v

let stop_value ((c, pos) : stop) =
  match pos with None -> color_value c | Some p -> Space [color_value c; value_of p]

let image_value : image -> value = function
  | `url u -> Url u
  | `linear (angle, stops) ->
      Function ("linear-gradient", Length (angle, Deg) :: List.map stop_value stops)
  | `radial (shape, (x, y), stops) ->
      let shape = match shape with
        | `circle -> [Keyword "circle"]
        | `ellipse -> [Keyword "ellipse"]
        | `size (w, h) -> [value_of w; value_of h] in
      Function ("radial-gradient",
        Space (shape @ [Keyword "at"; value_of x; value_of y]) :: List.map stop_value stops)

let image_or_value = function
  | #image as i -> image_value i
  | #simple as v -> value_of v

let time_value : time -> value = function
  | `ms f -> Length (f, Ms)
  | `s f -> Length (f, S)

let easing_value : easing -> value = function
  | `linear -> Keyword "linear" | `ease -> Keyword "ease" | `ease_in -> Keyword "ease-in"
  | `ease_out -> Keyword "ease-out" | `ease_in_out -> Keyword "ease-in-out"
  | `step_start -> Keyword "step-start" | `step_end -> Keyword "step-end"
  | `cubic_bezier (a, b, c, d) -> Function ("cubic-bezier", [Number a; Number b; Number c; Number d])
  | `steps n -> Function ("steps", [Integer n])

let deg d = Length (d, Deg)

let transform_value : transform_fn -> value = function
  | `translate (x, y) -> Function ("translate", [value_of x; value_of y])
  | `translate_x x -> Function ("translateX", [value_of x])
  | `translate_y y -> Function ("translateY", [value_of y])
  | `scale f -> Function ("scale", [Number f])
  | `scale_xy (x, y) -> Function ("scale", [Number x; Number y])
  | `scale_x f -> Function ("scaleX", [Number f])
  | `scale_y f -> Function ("scaleY", [Number f])
  | `rotate d -> Function ("rotate", [deg d])
  | `skew_x d -> Function ("skewX", [deg d])
  | `skew_y d -> Function ("skewY", [deg d])
  | `perspective l -> Function ("perspective", [value_of l])

let filter_value : filter_fn -> value = function
  | `blur l -> Function ("blur", [value_of l])
  | `brightness f -> Function ("brightness", [Number f])
  | `contrast f -> Function ("contrast", [Number f])
  | `grayscale f -> Function ("grayscale", [Number f])
  | `hue_rotate d -> Function ("hue-rotate", [deg d])
  | `invert f -> Function ("invert", [Number f])
  | `opacity f -> Function ("opacity", [Number f])
  | `saturate f -> Function ("saturate", [Number f])
  | `sepia f -> Function ("sepia", [Number f])
  | `drop_shadow (x, y, blur, c) ->
      Function ("drop-shadow", [Space [value_of x; value_of y; value_of blur; color_value c]])

let functions_or_none f = function
  | [] -> Keyword "none"
  | fs -> Space (List.map f fs)

let track_size_value : track_size -> value = function
  | `auto -> Keyword "auto"
  | `min_content -> Keyword "min-content"
  | `max_content -> Keyword "max-content"
  | #length as l -> value_of l

let rec track_value : track -> value = function
  | `minmax (a, b) -> Function ("minmax", [track_size_value a; track_size_value b])
  | `fit_content l -> Function ("fit-content", [value_of l])
  | `repeat (count, tracks) ->
      let count = match count with
        | `n n -> Integer n | `auto_fill -> Keyword "auto-fill" | `auto_fit -> Keyword "auto-fit" in
      Function ("repeat", [count; Space (List.map track_value tracks)])
  | #track_size as s -> track_size_value s

let grid_line_value : grid_line -> value = function
  | `auto -> Keyword "auto"
  | `line n -> Integer n
  | `span n -> Space [Keyword "span"; Integer n]
  | `between (a, b) -> Space [Integer a; Keyword "/"; Integer b]
  | `area name -> Keyword name

let position_value : position_xy -> value = function
  | `left -> Keyword "left" | `center -> Keyword "center" | `right -> Keyword "right"
  | `top -> Keyword "top" | `bottom -> Keyword "bottom"
  | #length as l -> value_of l

let clip_value : clip_shape -> value = function
  | `none -> Keyword "none"
  | `inset (t, r, b, l) -> Function ("inset", [Space [value_of t; value_of r; value_of b; value_of l]])
  | `circle (r, (x, y)) -> Function ("circle", [Space [value_of r; Keyword "at"; value_of x; value_of y]])
  | `polygon points ->
      Function ("polygon", List.map (fun (x, y) -> Space [value_of x; value_of y]) points)
  | `url u -> Url u

let shadow_value = function
  | `shadow (x, y, blur, c) -> Space [value_of x; value_of y; value_of blur; color_value c]
  | `spread (x, y, blur, s, c) -> Space [value_of x; value_of y; value_of blur; value_of s; color_value c]
  | `inner (x, y, blur, s, c) ->
      Space [Keyword "inset"; value_of x; value_of y; value_of blur; value_of s; color_value c]
  | #simple as v -> value_of v

(* ---------- Color manipulation ---------- *)

module Color = struct
  type t = color

  let named_rgb : named_color -> (int * int * int * float) option = function
    | `black -> Some (0, 0, 0, 1.) | `white -> Some (255, 255, 255, 1.)
    | `red -> Some (255, 0, 0, 1.) | `green -> Some (0, 128, 0, 1.) | `blue -> Some (0, 0, 255, 1.)
    | `yellow -> Some (255, 255, 0, 1.) | `orange -> Some (255, 165, 0, 1.)
    | `purple -> Some (128, 0, 128, 1.) | `pink -> Some (255, 192, 203, 1.)
    | `gray | `grey -> Some (128, 128, 128, 1.) | `silver -> Some (192, 192, 192, 1.)
    | `maroon -> Some (128, 0, 0, 1.) | `navy -> Some (0, 0, 128, 1.) | `teal -> Some (0, 128, 128, 1.)
    | `olive -> Some (128, 128, 0, 1.) | `lime -> Some (0, 255, 0, 1.)
    | `aqua | `cyan -> Some (0, 255, 255, 1.) | `fuchsia | `magenta -> Some (255, 0, 255, 1.)
    | `brown -> Some (165, 42, 42, 1.) | `gold -> Some (255, 215, 0, 1.)
    | `indigo -> Some (75, 0, 130, 1.) | `violet -> Some (238, 130, 238, 1.)
    | `transparent -> Some (0, 0, 0, 0.)
    | `current_color -> None

  let parse_hex s =
    let s = if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
    let digit c = match c with
      | '0' .. '9' -> Char.code c - 48
      | 'a' .. 'f' -> Char.code c - 87
      | 'A' .. 'F' -> Char.code c - 55
      | _ -> raise Exit in
    let short i = float_of_int (digit s.[i] * 17) /. 255. in
    let pair i = float_of_int (digit s.[i] * 16 + digit s.[i + 1]) /. 255. in
    try
      match String.length s with
      | 3 -> Some (short 0, short 1, short 2, 1.)
      | 4 -> Some (short 0, short 1, short 2, short 3)
      | 6 -> Some (pair 0, pair 2, pair 4, 1.)
      | 8 -> Some (pair 0, pair 2, pair 4, pair 6)
      | _ -> None
    with Exit -> None

  let pi = Float.pi
  let rad d = d *. pi /. 180.
  let deg r = r *. 180. /. pi
  let normalize_hue h = let h = Float.rem h 360. in if h < 0. then h +. 360. else h
  let clamp lo hi x = Float.max lo (Float.min hi x)

  (* sRGB transfer function, sign-preserving for out-of-gamut values. *)
  let to_linear c =
    let a = Float.abs c in
    Float.copy_sign (if a <= 0.04045 then a /. 12.92 else ((a +. 0.055) /. 1.055) ** 2.4) c
  let of_linear c =
    let a = Float.abs c in
    Float.copy_sign (if a <= 0.0031308 then a *. 12.92 else 1.055 *. (a ** (1. /. 2.4)) -. 0.055) c

  (* https://bottosson.github.io/posts/oklab/ *)
  let linear_srgb_to_oklab (r, g, b) =
    let l = Float.cbrt (0.4122214708 *. r +. 0.5363325363 *. g +. 0.0514459929 *. b) in
    let m = Float.cbrt (0.2119034982 *. r +. 0.6806995451 *. g +. 0.1073969566 *. b) in
    let s = Float.cbrt (0.0883024619 *. r +. 0.2817188376 *. g +. 0.6299787005 *. b) in
    ( 0.2104542553 *. l +. 0.7936177850 *. m -. 0.0040720468 *. s,
      1.9779984951 *. l -. 2.4285922050 *. m +. 0.4505937099 *. s,
      0.0259040371 *. l +. 0.7827717662 *. m -. 0.8086757660 *. s )

  let oklab_to_linear_srgb (l, a, b) =
    let cube x = x *. x *. x in
    let l' = cube (l +. 0.3963377774 *. a +. 0.2158037573 *. b) in
    let m' = cube (l -. 0.1055613458 *. a -. 0.0638541728 *. b) in
    let s' = cube (l -. 0.0894841775 *. a -. 1.2914855480 *. b) in
    ( 4.0767416621 *. l' -. 3.3077115913 *. m' +. 0.2309699292 *. s',
      -1.2684380046 *. l' +. 2.6097574011 *. m' -. 0.3413193965 *. s',
      -0.0041960863 *. l' -. 0.7034186147 *. m' +. 1.7076147010 *. s' )

  (* CIE Lab (D50) → XYZ D50 → XYZ D65 (Bradford) → linear sRGB, per CSS Color 4. *)
  let lab_to_linear_srgb (l, a, b) =
    let kappa = 24389. /. 27. and epsilon = 216. /. 24389. in
    let fy = (l +. 16.) /. 116. in
    let fx = a /. 500. +. fy and fz = fy -. b /. 200. in
    let f3 f = if f *. f *. f > epsilon then f *. f *. f else (116. *. f -. 16.) /. kappa in
    let x = f3 fx *. 0.3457 /. 0.3585 in
    let y = if l > kappa *. epsilon then fy *. fy *. fy else l /. kappa in
    let z = f3 fz *. (1. -. 0.3457 -. 0.3585) /. 0.3585 in
    let x65 = 0.955473421488075 *. x -. 0.02309845494876471 *. y +. 0.06325924320057072 *. z in
    let y65 = -0.0283697093338637 *. x +. 1.0099953980813041 *. y +. 0.021041441191917323 *. z in
    let z65 = 0.012314014864481998 *. x -. 0.020507649298898964 *. y +. 1.330365926242124 *. z in
    ( 3.2409699419045226 *. x65 -. 1.537383177570094 *. y65 -. 0.4986107602930034 *. z65,
      -0.9692436362808796 *. x65 +. 1.8759675015077202 *. y65 +. 0.04155505740717559 *. z65,
      0.05563007969699366 *. x65 -. 0.20397695888897652 *. y65 +. 1.0569715142428786 *. z65 )

  let hsl_to_srgb h s l =
    let h = normalize_hue h in
    let f n =
      let k = Float.rem (n +. h /. 30.) 12. in
      let a = s *. Float.min l (1. -. l) in
      l -. a *. Float.max (-1.) (Float.min (k -. 3.) (Float.min (9. -. k) 1.)) in
    (f 0., f 8., f 4.)

  let hwb_to_srgb h w b =
    let w = w /. 100. and b = b /. 100. in
    if w +. b >= 1. then let g = w /. (w +. b) in (g, g, g)
    else
      let (r, g, bl) = hsl_to_srgb h 1. 0.5 in
      let f c = c *. (1. -. w -. b) +. w in
      (f r, f g, f bl)

  let of_srgb (r, g, b) alpha =
    let (l, a, b) = linear_srgb_to_oklab (to_linear r, to_linear g, to_linear b) in
    Some (l, a, b, alpha)
  let of_rgb255 r g b alpha =
    of_srgb (float_of_int r /. 255., float_of_int g /. 255., float_of_int b /. 255.) alpha
  let of_lab (l, a, b) alpha =
    let (l, a, b) = linear_srgb_to_oklab (lab_to_linear_srgb (l, a, b)) in
    Some (l, a, b, alpha)
  let polar l c h = (l, c *. Float.cos (rad h), c *. Float.sin (rad h))

  let to_oklab : color -> (float * float * float * float) option = function
    | #named_color as n -> Option.bind (named_rgb n) (fun (r, g, b, a) -> of_rgb255 r g b a)
    | `hex s -> Option.bind (parse_hex s) (fun (r, g, b, a) -> of_srgb (r, g, b) a)
    | `rgb (r, g, b) -> of_rgb255 r g b 1.
    | `rgba (r, g, b, a) -> of_rgb255 r g b a
    | `hsl (h, s, l) ->
        of_srgb (hsl_to_srgb (float_of_int h) (float_of_int s /. 100.) (float_of_int l /. 100.)) 1.
    | `hsla (h, s, l, a) ->
        of_srgb (hsl_to_srgb (float_of_int h) (float_of_int s /. 100.) (float_of_int l /. 100.)) a
    | `hwb (h, w, b) -> of_srgb (hwb_to_srgb h w b) 1.
    | `lab (l, a, b) -> of_lab (l, a, b) 1.
    | `laba (l, a, b, alpha) -> of_lab (l, a, b) alpha
    | `lch (l, c, h) -> of_lab (polar l c h) 1.
    | `lcha (l, c, h, a) -> of_lab (polar l c h) a
    | `oklab (l, a, b) -> Some (l, a, b, 1.)
    | `oklaba (l, a, b, alpha) -> Some (l, a, b, alpha)
    | `oklch (l, c, h) -> let (l, a, b) = polar l c h in Some (l, a, b, 1.)
    | `oklcha (l, c, h, alpha) -> let (l, a, b) = polar l c h in Some (l, a, b, alpha)
    | `named _ | `var _ | `mix _ | `from _ -> None

  let to_oklch c =
    Option.map (fun (l, a, b, alpha) ->
      let c = Float.sqrt (a *. a +. b *. b) in
      let h = if c < 1e-6 then 0. else normalize_hue (deg (Float.atan2 b a)) in
      (l, c, h, alpha)) (to_oklab c)

  let to_srgb c =
    Option.map (fun (l, a, b, alpha) ->
      let (r, g, b) = oklab_to_linear_srgb (l, a, b) in
      (of_linear r, of_linear g, of_linear b, alpha)) (to_oklab c)

  let round4 x = Float.round (x *. 10000.) /. 10000.

  let oklch ?(alpha = 1.) l c h : color =
    let l = round4 l and c = round4 c and h = round4 h in
    if alpha >= 1. then `oklch (l, c, h) else `oklcha (l, c, h, round4 alpha)

  let to_hex c =
    Option.map (fun (r, g, b, a) ->
      (* tiny nudge: round-tripping through OKLab leaves values like 127.49999 *)
      let byte x = int_of_float (Float.round (clamp 0. 1. x *. 255. +. 1e-4)) in
      if a >= 1. then Printf.sprintf "#%02x%02x%02x" (byte r) (byte g) (byte b)
      else Printf.sprintf "#%02x%02x%02x%02x" (byte r) (byte g) (byte b) (byte a)) (to_srgb c)

  let hex c = match to_hex c with Some h -> `hex h | None -> c

  let num f =
    if Float.is_integer f then string_of_int (int_of_float f)
    else
      let s = Printf.sprintf "%.4f" f in
      let n = ref (String.length s) in
      while !n > 0 && s.[!n - 1] = '0' do decr n done;
      String.sub s 0 !n

  let oklab ?(alpha = 1.) l a b : color =
    let l = round4 l and a = round4 a and b = round4 b in
    if alpha >= 1. then `oklab (l, a, b) else `oklaba (l, a, b, round4 alpha)

  (* Channel operations: computed statically when the color is known,
     otherwise emitted as CSS relative color syntax. *)
  type op = [ `set of float | `add of float | `mul of float ]

  let apply (op : op option) v =
    match op with
    | None -> v
    | Some (`set x) -> x
    | Some (`add x) -> v +. x
    | Some (`mul x) -> v *. x

  let expr channel (op : op option) =
    match op with
    | None -> channel
    | Some (`set x) -> num x
    | Some (`add x) when x < 0. -> Printf.sprintf "calc(%s - %s)" channel (num (-. x))
    | Some (`add x) -> Printf.sprintf "calc(%s + %s)" channel (num x)
    | Some (`mul x) -> Printf.sprintf "calc(%s * %s)" channel (num x)

  let alpha_expr = function None -> None | op -> Some (expr "alpha" op)

  let adjust_oklab ?l ?a ?b ?alpha c =
    match to_oklab c with
    | Some (l0, a0, b0, x0) ->
        oklab ~alpha:(clamp 0. 1. (apply alpha x0)) (clamp 0. 1. (apply l l0)) (apply a a0) (apply b b0)
    | None -> `from (`oklab, c, expr "l" l, expr "a" a, expr "b" b, alpha_expr alpha)

  let adjust_oklch ?l ?c ?h ?alpha col =
    match to_oklch col with
    | Some (l0, c0, h0, x0) ->
        oklch ~alpha:(clamp 0. 1. (apply alpha x0))
          (clamp 0. 1. (apply l l0)) (Float.max 0. (apply c c0)) (normalize_hue (apply h h0))
    | None -> `from (`oklch, col, expr "l" l, expr "c" c, expr "h" h, alpha_expr alpha)

  let lighten amount c = adjust_oklch ~l:(`add amount) c
  let darken amount c = lighten (-. amount) c
  let saturate amount c = adjust_oklch ~c:(`mul (1. +. amount)) c
  let desaturate amount c = saturate (-. amount) c
  let rotate degrees c = adjust_oklch ~h:(`add degrees) c
  let complement c = rotate 180. c
  let grayscale c = adjust_oklch ~c:(`set 0.) c
  let with_alpha alpha c = adjust_oklch ~alpha:(`set alpha) c

  let invert c =
    match to_srgb c with
    | Some (r, g, b, a) ->
        (match of_srgb (1. -. clamp 0. 1. r, 1. -. clamp 0. 1. g, 1. -. clamp 0. 1. b) a with
         | Some (l, a', b', alpha) ->
             let ch = Float.sqrt (a' *. a' +. b' *. b') in
             oklch ~alpha l ch (if ch < 1e-6 then 0. else normalize_hue (deg (Float.atan2 b' a')))
         | None -> c)
    | None -> `from (`rgb, c, "calc(255 - r)", "calc(255 - g)", "calc(255 - b)", None)

  let lerp t a b = a +. (b -. a) *. t

  let mix ?(space = `oklab) ?(weight = 0.5) a b =
    (* [weight] is the proportion of [a]. *)
    let t = 1. -. weight in
    let from_oklab (l, a', b', alpha) =
      let ch = Float.sqrt (a' *. a' +. b' *. b') in
      oklch ~alpha l ch (if ch < 1e-6 then 0. else normalize_hue (deg (Float.atan2 b' a'))) in
    match space, to_oklab a, to_oklab b with
    | `oklab, Some (l1, a1, b1, x1), Some (l2, a2, b2, x2) ->
        from_oklab (lerp t l1 l2, lerp t a1 a2, lerp t b1 b2, lerp t x1 x2)
    | `oklch, Some _, Some _ ->
        let (l1, c1, h1, x1) = Option.get (to_oklch a) and (l2, c2, h2, x2) = Option.get (to_oklch b) in
        (* shorter hue arc; a powerless (gray) hue takes the other one *)
        let h1 = if c1 < 1e-6 then h2 else h1 and h2 = if c2 < 1e-6 then h1 else h2 in
        let dh = let d = h2 -. h1 in if d > 180. then d -. 360. else if d < -180. then d +. 360. else d in
        oklch ~alpha:(lerp t x1 x2) (lerp t l1 l2) (lerp t c1 c2) (normalize_hue (h1 +. dh *. t))
    | `srgb, Some _, Some _ ->
        let (r1, g1, b1, x1) = Option.get (to_srgb a) and (r2, g2, b2, x2) = Option.get (to_srgb b) in
        (match of_srgb (lerp t r1 r2, lerp t g1 g2, lerp t b1 b2) (lerp t x1 x2) with
         | Some lab -> from_oklab lab
         | None -> `mix (space, a, weight *. 100., b))
    | _ -> `mix (space, a, weight *. 100., b)

  (* WCAG 2 relative luminance and contrast ratio. *)
  let luminance c =
    Option.map (fun (r, g, b, _) ->
      let lin x = to_linear (clamp 0. 1. x) in
      0.2126 *. lin r +. 0.7152 *. lin g +. 0.0722 *. lin b) (to_srgb c)

  let contrast a b =
    match luminance a, luminance b with
    | Some la, Some lb ->
        let hi = Float.max la lb and lo = Float.min la lb in
        Some ((hi +. 0.05) /. (lo +. 0.05))
    | _ -> None

  let readable_on ?(light = (`white : color)) ?(dark = (`black : color)) background =
    match contrast background light, contrast background dark with
    | Some cl, Some cd -> if cl >= cd then light else dark
    | _ -> dark
end

(* ---------- Building ---------- *)

let decl property value = { property; value; important = false }
let important d = { d with important = true }
let prop property value = decl property (Raw value)
let custom name v = decl ("--" ^ name) (value_of v)

module Var = struct
  type t = { name : string; value : value }
  let make name v = { name; value = value_of v }
  let name t = t.name
  let use t = `var t.name
  let declare t = decl ("--" ^ t.name) t.value
  let declarations ts = List.map declare ts
end


let font_face declarations = Font_face declarations
let import ?media url = Import (url, media)
let raw css = Raw_css css

let keyframes name frames =
  let sel = function `from -> From | `to_ -> To | `at p -> At p in
  Keyframes (name, List.map (fun (sels, decls) -> (List.map sel sels, decls)) frames)


module Keyframes = struct
  type t = { name : string; frames : (keyframe_selector list * declaration list) list }
  let make name frames =
    let sel = function `from -> From | `to_ -> To | `at p -> At p in
    { name; frames = List.map (fun (sels, decls) -> (List.map sel sels, decls)) frames }
  let name t = t.name
  let declare t = Keyframes (t.name, t.frames)
end

module Transition = struct
  type property =
    [ `all
    | `color | `background_color | `border_color | `border_top_color | `border_right_color
    | `border_bottom_color | `border_left_color | `outline_color | `accent_color
    | `caret_color | `text_decoration_color | `fill | `stroke | `width | `height
    | `min_width | `min_height | `top | `right | `bottom | `left | `inset
    | `max_width | `max_height | `margin | `margin_top | `margin_right | `margin_bottom
    | `margin_left | `margin_inline | `margin_block | `padding | `padding_top
    | `padding_right | `padding_bottom | `padding_left | `padding_inline | `padding_block
    | `margin_block_start | `margin_block_end | `margin_inline_start | `margin_inline_end
    | `padding_block_start | `padding_block_end | `padding_inline_start | `padding_inline_end
    | `gap | `row_gap | `column_gap | `border_radius | `outline_offset | `text_indent
    | `letter_spacing | `word_spacing | `border_width | `outline_width | `flex_basis
    | `font_size | `line_height | `font_weight | `font_style | `display | `position
    | `box_sizing | `overflow | `overflow_x | `overflow_y | `flex_direction
    | `flex_wrap | `justify_content | `justify_items | `justify_self | `align_items
    | `align_self | `align_content | `place_items | `place_content | `place_self
    | `text_align | `text_decoration | `text_decoration_line | `text_transform
    | `white_space | `text_overflow | `word_break | `overflow_wrap | `list_style
    | `list_style_type | `list_style_position | `cursor | `pointer_events
    | `user_select | `visibility | `border_style | `border_collapse | `background_repeat
    | `background_size | `background_image | `background | `background_clip
    | `background_attachment | `font_variant_numeric | `object_fit | `vertical_align
    | `resize | `scroll_behavior | `float_ | `clear | `table_layout | `font_display
    | `content | `aspect_ratio | `flex | `z_index | `order | `opacity | `flex_grow
    | `flex_shrink | `border | `border_top | `border_right | `border_bottom
    | `border_left | `outline | `box_shadow | `font_family | `transition | `transform
    | `animation | `filter | `backdrop_filter | `clip_path | `will_change
    | `grid_template_columns | `grid_template_rows | `grid_template_areas
    | `grid_area | `grid_column | `grid_row | `background_position | `transform_origin
    | `unicode_range
    | `custom of string ]

  let string_of_property : property -> string = function
    | `all -> "all"
    | `color -> "color"
    | `background_color -> "background-color"
    | `border_color -> "border-color"
    | `border_top_color -> "border-top-color"
    | `border_right_color -> "border-right-color"
    | `border_bottom_color -> "border-bottom-color"
    | `border_left_color -> "border-left-color"
    | `outline_color -> "outline-color"
    | `accent_color -> "accent-color"
    | `caret_color -> "caret-color"
    | `text_decoration_color -> "text-decoration-color"
    | `fill -> "fill"
    | `stroke -> "stroke"
    | `width -> "width"
    | `height -> "height"
    | `min_width -> "min-width"
    | `min_height -> "min-height"
    | `top -> "top"
    | `right -> "right"
    | `bottom -> "bottom"
    | `left -> "left"
    | `inset -> "inset"
    | `max_width -> "max-width"
    | `max_height -> "max-height"
    | `margin -> "margin"
    | `margin_top -> "margin-top"
    | `margin_right -> "margin-right"
    | `margin_bottom -> "margin-bottom"
    | `margin_left -> "margin-left"
    | `margin_inline -> "margin-inline"
    | `margin_block -> "margin-block"
    | `padding -> "padding"
    | `padding_top -> "padding-top"
    | `padding_right -> "padding-right"
    | `padding_bottom -> "padding-bottom"
    | `padding_left -> "padding-left"
    | `padding_inline -> "padding-inline"
    | `padding_block -> "padding-block"
    | `margin_block_start -> "margin-block-start"
    | `margin_block_end -> "margin-block-end"
    | `margin_inline_start -> "margin-inline-start"
    | `margin_inline_end -> "margin-inline-end"
    | `padding_block_start -> "padding-block-start"
    | `padding_block_end -> "padding-block-end"
    | `padding_inline_start -> "padding-inline-start"
    | `padding_inline_end -> "padding-inline-end"
    | `gap -> "gap"
    | `row_gap -> "row-gap"
    | `column_gap -> "column-gap"
    | `border_radius -> "border-radius"
    | `outline_offset -> "outline-offset"
    | `text_indent -> "text-indent"
    | `letter_spacing -> "letter-spacing"
    | `word_spacing -> "word-spacing"
    | `border_width -> "border-width"
    | `outline_width -> "outline-width"
    | `flex_basis -> "flex-basis"
    | `font_size -> "font-size"
    | `line_height -> "line-height"
    | `font_weight -> "font-weight"
    | `font_style -> "font-style"
    | `display -> "display"
    | `position -> "position"
    | `box_sizing -> "box-sizing"
    | `overflow -> "overflow"
    | `overflow_x -> "overflow-x"
    | `overflow_y -> "overflow-y"
    | `flex_direction -> "flex-direction"
    | `flex_wrap -> "flex-wrap"
    | `justify_content -> "justify-content"
    | `justify_items -> "justify-items"
    | `justify_self -> "justify-self"
    | `align_items -> "align-items"
    | `align_self -> "align-self"
    | `align_content -> "align-content"
    | `place_items -> "place-items"
    | `place_content -> "place-content"
    | `place_self -> "place-self"
    | `text_align -> "text-align"
    | `text_decoration -> "text-decoration"
    | `text_decoration_line -> "text-decoration-line"
    | `text_transform -> "text-transform"
    | `white_space -> "white-space"
    | `text_overflow -> "text-overflow"
    | `word_break -> "word-break"
    | `overflow_wrap -> "overflow-wrap"
    | `list_style -> "list-style"
    | `list_style_type -> "list-style-type"
    | `list_style_position -> "list-style-position"
    | `cursor -> "cursor"
    | `pointer_events -> "pointer-events"
    | `user_select -> "user-select"
    | `visibility -> "visibility"
    | `border_style -> "border-style"
    | `border_collapse -> "border-collapse"
    | `background_repeat -> "background-repeat"
    | `background_size -> "background-size"
    | `background_image -> "background-image"
    | `background -> "background"
    | `background_clip -> "background-clip"
    | `background_attachment -> "background-attachment"
    | `font_variant_numeric -> "font-variant-numeric"
    | `object_fit -> "object-fit"
    | `vertical_align -> "vertical-align"
    | `resize -> "resize"
    | `scroll_behavior -> "scroll-behavior"
    | `float_ -> "float"
    | `clear -> "clear"
    | `table_layout -> "table-layout"
    | `font_display -> "font-display"
    | `content -> "content"
    | `aspect_ratio -> "aspect-ratio"
    | `flex -> "flex"
    | `z_index -> "z-index"
    | `order -> "order"
    | `opacity -> "opacity"
    | `flex_grow -> "flex-grow"
    | `flex_shrink -> "flex-shrink"
    | `border -> "border"
    | `border_top -> "border-top"
    | `border_right -> "border-right"
    | `border_bottom -> "border-bottom"
    | `border_left -> "border-left"
    | `outline -> "outline"
    | `box_shadow -> "box-shadow"
    | `font_family -> "font-family"
    | `transition -> "transition"
    | `transform -> "transform"
    | `animation -> "animation"
    | `filter -> "filter"
    | `backdrop_filter -> "backdrop-filter"
    | `clip_path -> "clip-path"
    | `will_change -> "will-change"
    | `grid_template_columns -> "grid-template-columns"
    | `grid_template_rows -> "grid-template-rows"
    | `grid_template_areas -> "grid-template-areas"
    | `grid_area -> "grid-area"
    | `grid_column -> "grid-column"
    | `grid_row -> "grid-row"
    | `background_position -> "background-position"
    | `transform_origin -> "transform-origin"
    | `unicode_range -> "unicode-range"
    | `custom name -> name

  type t = value

  let make ?(duration = `ms 150.) ?(easing = `ease) ?delay property : t =
    Space ([Keyword (string_of_property property); time_value duration; easing_value easing]
           @ Option.to_list (Option.map time_value delay))
end

module Animation = struct
  type t = value

  let make ?(duration = `ms 300.) ?(easing = `ease) ?delay ?iterations ?direction ?fill ?play_state
      (keyframes : Keyframes.t) : t =
    let kw k = Keyword k in
    Space
      ([time_value duration; easing_value easing]
       @ Option.to_list (Option.map time_value delay)
       @ Option.to_list (Option.map (function
           | `infinite -> kw "infinite" | `times n -> Number n) iterations)
       @ Option.to_list (Option.map (function
           | `normal -> kw "normal" | `reverse -> kw "reverse"
           | `alternate -> kw "alternate" | `alternate_reverse -> kw "alternate-reverse") direction)
       @ Option.to_list (Option.map (function
           | `none -> kw "none" | `forwards -> kw "forwards"
           | `backwards -> kw "backwards" | `both -> kw "both") fill)
       @ Option.to_list (Option.map (function `running -> kw "running" | `paused -> kw "paused") play_state)
       @ [kw (Keyframes.name keyframes)])
end

let font_src sources =
  let src = function
    | `url u -> Url u
    | `url_format (u, f) -> Space [Url u; Function ("format", [String f])]
    | `local name -> Function ("local", [String name]) in
  decl "src" (Comma (List.map src sources))

(* ---------- Printing ---------- *)

let string_of_float f =
  if Float.is_integer f && Float.abs f < 1e15 then string_of_int (int_of_float f)
  else
    let s = Printf.sprintf "%.4f" f in
    let len = ref (String.length s) in
    while !len > 0 && s.[!len - 1] = '0' do decr len done;
    if !len > 0 && s.[!len - 1] = '.' then decr len;
    String.sub s 0 !len

let string_of_unit = function
  | Px -> "px" | Em -> "em" | Rem -> "rem" | Vh -> "vh" | Vw -> "vw"
  | Vmin -> "vmin" | Vmax -> "vmax" | Ch -> "ch" | Pt -> "pt" | Fr -> "fr" | Deg -> "deg" | Ms -> "ms" | S -> "s"

let string_of_color ?(minify = false) c =
  let sep = if minify then "," else ", " in
  let pct f = string_of_float f ^ "%" in
  (* Space-separated syntax with optional [/ alpha]. *)
  let modern name x y z alpha =
    name ^ "(" ^ x ^ " " ^ y ^ " " ^ z
    ^ (match alpha with None -> "" | Some a -> " / " ^ string_of_float a) ^ ")" in
  match c with
  | Hex h -> "#" ^ h
  | Rgb (r, g, b, None) -> Printf.sprintf "rgb(%d%s%d%s%d)" r sep g sep b
  | Rgb (r, g, b, Some a) -> Printf.sprintf "rgba(%d%s%d%s%d%s%s)" r sep g sep b sep (string_of_float a)
  | Hsl (h, s, l, None) -> Printf.sprintf "hsl(%d%s%d%%%s%d%%)" h sep s sep l
  | Hsl (h, s, l, Some a) -> Printf.sprintf "hsla(%d%s%d%%%s%d%%%s%s)" h sep s sep l sep (string_of_float a)
  | Hwb (h, w, b, a) -> modern "hwb" (string_of_float h) (pct w) (pct b) a
  | Lab (l, a, b, alpha) -> modern "lab" (pct l) (string_of_float a) (string_of_float b) alpha
  | Lch (l, c, h, a) -> modern "lch" (pct l) (string_of_float c) (string_of_float h) a
  | Oklab (l, a, b, alpha) -> modern "oklab" (string_of_float l) (string_of_float a) (string_of_float b) alpha
  | Oklch (l, c, h, a) -> modern "oklch" (string_of_float l) (string_of_float c) (string_of_float h) a

let rec value_to_string ?(minify = false) v =
  let comma = if minify then "," else ", " in
  let value_to_string = value_to_string ~minify in
  match v with
  | Keyword k -> k
  | Integer n -> string_of_int n
  | Number f -> string_of_float f
  | Length (f, u) -> string_of_float f ^ string_of_unit u
  | Percentage f -> string_of_float f ^ "%"
  | Color c -> string_of_color ~minify c
  | String s -> Selector.quote s
  | Url u -> "url(" ^ Selector.quote u ^ ")"
  | Var (name, None) -> "var(--" ^ name ^ ")"
  | Var (name, Some fallback) -> "var(--" ^ name ^ comma ^ value_to_string fallback ^ ")"
  | Function (name, args) -> name ^ "(" ^ String.concat comma (List.map value_to_string args) ^ ")"
  | Space vs -> String.concat " " (List.map value_to_string vs)
  | Comma vs -> String.concat comma (List.map value_to_string vs)
  | Raw s -> s

let declaration_to_string ?(minify = false) d =
  d.property ^ (if minify then ":" else ": ") ^ value_to_string ~minify d.value
  ^ (if d.important then (if minify then "!important" else " !important") else "")

let inline declarations =
  String.concat "; " (List.map declaration_to_string declarations)

let undefined_vars (sheet : stylesheet) =
  let used = Hashtbl.create 16 and defined = Hashtbl.create 16 in
  let rec value = function
    | Var (name, fallback) -> Hashtbl.replace used name (); Option.iter value fallback
    | Function (_, vs) | Space vs | Comma vs -> List.iter value vs
    | _ -> () in
  let declarations =
    List.iter (fun d ->
      let p = d.property in
      if String.length p > 2 && String.sub p 0 2 = "--" then
        Hashtbl.replace defined (String.sub p 2 (String.length p - 2)) ();
      value d.value) in
  let rec stmt = function
    | Rule (_, ds) | Font_face ds -> declarations ds
    | Media (_, body) | Supports (_, body) -> List.iter stmt body
    | Keyframes (_, frames) -> List.iter (fun (_, ds) -> declarations ds) frames
    | Import _ | Raw_css _ -> () in
  List.iter stmt sheet;
  Hashtbl.fold (fun name () acc -> if Hashtbl.mem defined name then acc else name :: acc) used []
  |> List.sort compare

let string_of_keyframe_selector = function
  | From -> "from" | To -> "to" | At p -> string_of_float p ^ "%"

let to_string ?(minify = false) (sheet : stylesheet) =
  let buf = Buffer.create 4096 in
  let add s = Buffer.add_string buf s in
  let indent depth = if not minify then add (String.make (2 * depth) ' ') in
  let newline () = if not minify then add "\n" in
  let block depth header body =
    indent depth; add header; add (if minify then "{" else " {"); newline ();
    body (depth + 1);
    indent depth; add "}"; newline () in
  let declarations depth ds =
    let n = List.length ds in
    List.iteri (fun i d ->
      indent depth;
      add (declaration_to_string ~minify d);
      if not minify || i < n - 1 then add ";";
      newline ()) ds in
  let rec stmt depth = function
    | Rule (sels, ds) ->
        block depth (Selector.print_list ~minify sels) (fun d -> declarations d ds)
    | Media (q, body) -> block depth ("@media " ^ q) (fun d -> List.iter (stmt d) body)
    | Supports (c, body) -> block depth ("@supports " ^ c) (fun d -> List.iter (stmt d) body)
    | Font_face ds -> block depth "@font-face" (fun d -> declarations d ds)
    | Keyframes (name, frames) ->
        block depth ("@keyframes " ^ name) (fun d ->
          List.iter (fun (sels, ds) ->
            block d (String.concat (if minify then "," else ", ") (List.map string_of_keyframe_selector sels))
              (fun d -> declarations d ds)) frames)
    | Import (url, media) ->
        indent depth;
        add ("@import " ^ value_to_string (Url url));
        Option.iter (fun m -> add (" " ^ m)) media;
        add ";"; newline ()
    | Raw_css css -> indent depth; add css; newline () in
  List.iter (stmt 0) sheet;
  Buffer.contents buf

(* ---------- At-rules with typed conditions ---------- *)

type media_feature =
  [ `screen | `print
  | `min_width of length | `max_width of length
  | `min_height of length | `max_height of length
  | `orientation of [ `portrait | `landscape ]
  | `prefers_color_scheme of [ `light | `dark ]
  | `prefers_reduced_motion of [ `reduce | `no_preference ]
  | `hover of [ `hover | `none ]
  | `pointer of [ `fine | `coarse | `none ] ]

let media_feature_to_string : media_feature -> string =
  let feature name v = "(" ^ name ^ ": " ^ v ^ ")" in
  let len l = value_to_string (value_of l) in
  function
  | `screen -> "screen"
  | `print -> "print"
  | `min_width l -> feature "min-width" (len l)
  | `max_width l -> feature "max-width" (len l)
  | `min_height l -> feature "min-height" (len l)
  | `max_height l -> feature "max-height" (len l)
  | `orientation o -> feature "orientation" (match o with `portrait -> "portrait" | `landscape -> "landscape")
  | `prefers_color_scheme s -> feature "prefers-color-scheme" (match s with `light -> "light" | `dark -> "dark")
  | `prefers_reduced_motion m ->
      feature "prefers-reduced-motion" (match m with `reduce -> "reduce" | `no_preference -> "no-preference")
  | `hover h -> feature "hover" (match h with `hover -> "hover" | `none -> "none")
  | `pointer p -> feature "pointer" (match p with `fine -> "fine" | `coarse -> "coarse" | `none -> "none")

(* All features must hold. *)
let media features stmts =
  Media (String.concat " and " (List.map media_feature_to_string features), stmts)

(* All declarations must be supported. *)
let supports declarations stmts =
  Supports
    (String.concat " and " (List.map (fun d -> "(" ^ declaration_to_string d ^ ")") declarations), stmts)

(* ---------- Optimization passes ---------- *)

module Optimize = struct
  (* Apply [f] to every statement list, innermost first. *)
  let rec map_blocks f (sheet : stylesheet) : stylesheet =
    f (List.map (function
      | Media (q, body) -> Media (q, map_blocks f body)
      | Supports (c, body) -> Supports (c, map_blocks f body)
      | s -> s) sheet)

  (* Apply [f] to every declaration list. *)
  let map_declarations f =
    map_blocks (List.map (function
      | Rule (sels, ds) -> Rule (sels, f ds)
      | Font_face ds -> Font_face (f ds)
      | Keyframes (name, frames) -> Keyframes (name, List.map (fun (s, ds) -> (s, f ds)) frames)
      | s -> s))

  let remove_empty =
    map_blocks (List.filter (function
      | Rule (_, []) | Rule ([], _) | Media (_, []) | Supports (_, []) | Font_face [] | Keyframes (_, []) -> false
      | _ -> true))

  let shorten_hex h =
    let h = String.lowercase_ascii h in
    let pairs = String.length h = 6 || String.length h = 8 in
    let rec same i = i >= String.length h || (h.[i] = h.[i + 1] && same (i + 2)) in
    if pairs && same 0 then String.init (String.length h / 2) (fun i -> h.[2 * i]) else h

  let rec minify_value = function
    | Length (0., (Px | Em | Rem | Vh | Vw | Vmin | Vmax | Ch | Pt)) -> Integer 0
    | Number f when Float.is_integer f && Float.abs f < 1e15 -> Integer (int_of_float f)
    | Color (Hex h) -> Color (Hex (shorten_hex h))
    | Space vs -> Space (List.map minify_value vs)
    | Comma vs -> Comma (List.map minify_value vs)
    | Function (n, vs) -> Function (n, List.map minify_value vs)
    | Var (n, Some v) -> Var (n, Some (minify_value v))
    | v -> v

  (** [0px] → [0], [#AABBCC] → [#abc], [1.0] → [1]. *)
  let minify_values = map_declarations (List.map (fun d -> { d with value = minify_value d.value }))

  (** Keeps only the winning declaration for each property (respecting
      [!important]). Drops intentional fallbacks such as
      [display: -webkit-box; display: flex]. *)
  let dedupe_declarations =
    map_declarations (fun ds ->
      List.fold_left (fun acc d ->
        match List.find_opt (fun e -> e.property = d.property) acc with
        | Some e when e.important && not d.important -> acc
        | Some _ -> List.filter (fun e -> e.property <> d.property) acc @ [d]
        | None -> acc @ [d]) [] ds)

  let dedupe_selectors =
    map_blocks (List.map (function
      | Rule (sels, ds) ->
          Rule (List.rev (List.fold_left (fun acc s -> if List.mem s acc then acc else s :: acc) [] sels), ds)
      | s -> s))

  (** Merges adjacent rules with the same selectors. *)
  (* Can two properties affect each other? Exact match, [all], a
     shorthand/longhand pair by prefix ([margin] / [margin-top]) or one of the
     shorthands that don't share a prefix. Errs on the side of "yes". *)
  let shorthands =
    [ "inset", ["top"; "right"; "bottom"; "left"]
    ; "gap", ["row-gap"; "column-gap"]
    ; "place-items", ["align-items"; "justify-items"]
    ; "place-content", ["align-content"; "justify-content"]
    ; "place-self", ["align-self"; "justify-self"]
    ; "font", ["line-height"] ]

  let related p q =
    let prefix a b =
      String.length b > String.length a + 1 && String.sub b 0 (String.length a + 1) = a ^ "-" in
    p = q || p = "all" || q = "all" || prefix p q || prefix q p
    || List.exists (fun (s, ls) -> (p = s && List.mem q ls) || (q = s && List.mem p ls)) shorthands

  (* Can two selectors never match the same element? Only the subject (last
     compound) matters. Sound but incomplete: [false] means "maybe". *)
  let exclusive_pseudos : (selector pseudo * selector pseudo) list =
    [ `enabled, `disabled; `valid, `invalid; `read_only, `read_write
    ; `required, `optional; `link, `visited ]

  let subject ((first, rest) : selector) =
    match List.rev rest with [] -> first | (_, c) :: _ -> c

  (* Does compound [b] satisfy every simple selector in compound [c]? *)
  let implies b c =
    (match c.tag with None | Some "*" -> true | Some t -> b.tag = Some t)
    && (c.id = None || c.id = b.id)
    && List.for_all (fun k -> List.mem k b.classes) c.classes
    && List.for_all (fun x -> List.mem x b.attrs) c.attrs
    && List.for_all (fun p -> List.mem p b.pseudos) c.pseudos
    && c.pseudo_element = None

  let compound_disjoint a b =
    let tag c = match c.tag with None | Some "*" -> None | Some t -> Some (String.lowercase_ascii t) in
    let differ f = match f a, f b with Some x, Some y -> x <> y | _ -> false in
    let equals_values c name =
      List.filter_map (fun (n, op) -> match op with `equals v when n = name -> Some v | _ -> None) c.attrs in
    (* a has :not(x) with x a single compound that b satisfies *)
    let negates a b =
      List.exists (function
        | `not sels -> List.exists (fun (c, rest) -> rest = [] && implies b c) sels
        | _ -> false) a.pseudos in
    differ tag
    || differ (fun c -> c.id)
    || a.pseudo_element <> b.pseudo_element
    || List.exists (fun (n, op) -> match op with
         | `equals v -> List.exists (fun v' -> v <> v') (equals_values b n)
         | _ -> false) a.attrs
    || List.exists (fun (p, q) ->
         (List.mem p a.pseudos && List.mem q b.pseudos) || (List.mem q a.pseudos && List.mem p b.pseudos))
         exclusive_pseudos
    || negates a b || negates b a

  let disjoint s1 s2 = compound_disjoint (subject s1) (subject s2)

  (* Order only matters between declarations of related properties with the
     same importance, from selectors with equal specificity that can match
     the same element. *)
  let conflict (sels1, ds1) (sels2, ds2) =
    List.exists (fun s1 ->
      List.exists (fun s2 ->
        Selector.selector_specificity s1 = Selector.selector_specificity s2 && not (disjoint s1 s2))
        sels2) sels1
    && List.exists (fun d1 ->
         List.exists (fun d2 -> d1.important = d2.important && related d1.property d2.property) ds2) ds1

  let rec rules_in = function
    | Rule (sels, ds) -> [(sels, ds)]
    | Media (_, body) | Supports (_, body) -> List.concat_map rules_in body
    | _ -> []

  let rec opaque = function
    | Raw_css _ -> true
    | Media (_, body) | Supports (_, body) -> List.exists opaque body
    | _ -> false

  (* Can the declarations of [rule] move earlier, past statement [m]? *)
  let can_cross rule m = not (opaque m || List.exists (fun r -> conflict r rule) (rules_in m))

  (* Merge each rule into the closest earlier rule accepted by [target],
     if its declarations can move up past everything in between. *)
  let merge_into ~target ~combine =
    map_blocks (fun stmts ->
      let step acc s =
        match s with
        | Rule (sels, ds) ->
            (* [acc] is reversed: most recent statement first. *)
            let rec go passed = function
              | [] -> None
              | Rule (s1, d1) :: rest when target (sels, ds) (s1, d1) ->
                  Some (List.rev_append passed (combine (s1, d1) (sels, ds) :: rest))
              | m :: rest -> if can_cross (sels, ds) m then go (m :: passed) rest else None in
            (match go [] acc with Some acc -> acc | None -> s :: acc)
        | _ -> s :: acc in
      List.rev (List.fold_left step [] stmts))

  let merge_same_selectors =
    merge_into
      ~target:(fun (sels, _) (s1, _) -> s1 = sels)
      ~combine:(fun (s1, d1) (_, ds) -> Rule (s1, d1 @ ds))

  let merge_same_declarations =
    merge_into
      ~target:(fun (_, ds) (_, d1) -> ds <> [] && d1 = ds)
      ~combine:(fun (s1, d1) (sels, _) -> Rule (s1 @ sels, d1))

  let all sheet =
    sheet
    |> minify_values
    |> merge_same_selectors
    |> dedupe_declarations
    |> dedupe_selectors
    |> merge_same_declarations
    |> dedupe_selectors
    |> remove_empty
end

(* ---------- Properties (last: they shadow common names) ---------- *)

let color v = decl "color" (value_of v)
let background_color v = decl "background-color" (value_of v)
let border_color v = decl "border-color" (value_of v)
let border_top_color v = decl "border-top-color" (value_of v)
let border_right_color v = decl "border-right-color" (value_of v)
let border_bottom_color v = decl "border-bottom-color" (value_of v)
let border_left_color v = decl "border-left-color" (value_of v)
let outline_color v = decl "outline-color" (value_of v)
let accent_color v = decl "accent-color" (value_of v)
let caret_color v = decl "caret-color" (value_of v)
let text_decoration_color v = decl "text-decoration-color" (value_of v)
let fill v = decl "fill" (value_of v)
let stroke v = decl "stroke" (value_of v)
let width v = decl "width" (value_of v)
let height v = decl "height" (value_of v)
let min_width v = decl "min-width" (value_of v)
let min_height v = decl "min-height" (value_of v)
let top v = decl "top" (value_of v)
let right v = decl "right" (value_of v)
let bottom v = decl "bottom" (value_of v)
let left v = decl "left" (value_of v)
let inset v = decl "inset" (value_of v)
let max_width v = decl "max-width" (value_of v)
let max_height v = decl "max-height" (value_of v)
let margin v = decl "margin" (value_of v)
let margin_top v = decl "margin-top" (value_of v)
let margin_right v = decl "margin-right" (value_of v)
let margin_bottom v = decl "margin-bottom" (value_of v)
let margin_left v = decl "margin-left" (value_of v)
let margin_inline v = decl "margin-inline" (value_of v)
let margin_block v = decl "margin-block" (value_of v)
let margin_block_start v = decl "margin-block-start" (value_of v)
let margin_block_end v = decl "margin-block-end" (value_of v)
let margin_inline_start v = decl "margin-inline-start" (value_of v)
let margin_inline_end v = decl "margin-inline-end" (value_of v)
let padding v = decl "padding" (value_of v)
let padding_top v = decl "padding-top" (value_of v)
let padding_right v = decl "padding-right" (value_of v)
let padding_bottom v = decl "padding-bottom" (value_of v)
let padding_left v = decl "padding-left" (value_of v)
let padding_inline v = decl "padding-inline" (value_of v)
let padding_block v = decl "padding-block" (value_of v)
let padding_block_start v = decl "padding-block-start" (value_of v)
let padding_block_end v = decl "padding-block-end" (value_of v)
let padding_inline_start v = decl "padding-inline-start" (value_of v)
let padding_inline_end v = decl "padding-inline-end" (value_of v)
let gap v = decl "gap" (value_of v)
let row_gap v = decl "row-gap" (value_of v)
let column_gap v = decl "column-gap" (value_of v)
let border_radius v = decl "border-radius" (value_of v)
let outline_offset v = decl "outline-offset" (value_of v)
let text_indent v = decl "text-indent" (value_of v)
let letter_spacing v = decl "letter-spacing" (value_of v)
let word_spacing v = decl "word-spacing" (value_of v)
let border_width v = decl "border-width" (value_of v)
let outline_width v = decl "outline-width" (value_of v)
let flex_basis v = decl "flex-basis" (value_of v)
let font_size v = decl "font-size" (value_of v)
let line_height v = decl "line-height" (value_of v)
let font_weight v = decl "font-weight" (value_of v)
let font_style v = decl "font-style" (value_of v)
let display v = decl "display" (value_of v)
let position v = decl "position" (value_of v)
let box_sizing v = decl "box-sizing" (value_of v)
let overflow v = decl "overflow" (value_of v)
let overflow_x v = decl "overflow-x" (value_of v)
let overflow_y v = decl "overflow-y" (value_of v)
let flex_direction v = decl "flex-direction" (value_of v)
let flex_wrap v = decl "flex-wrap" (value_of v)
let justify_content v = decl "justify-content" (value_of v)
let justify_items v = decl "justify-items" (value_of v)
let justify_self v = decl "justify-self" (value_of v)
let align_items v = decl "align-items" (value_of v)
let align_self v = decl "align-self" (value_of v)
let align_content v = decl "align-content" (value_of v)
let place_items v = decl "place-items" (value_of v)
let place_content v = decl "place-content" (value_of v)
let place_self v = decl "place-self" (value_of v)
let text_align v = decl "text-align" (value_of v)
let text_decoration v = decl "text-decoration" (value_of v)
let text_decoration_line v = decl "text-decoration-line" (value_of v)
let text_transform v = decl "text-transform" (value_of v)
let white_space v = decl "white-space" (value_of v)
let text_overflow v = decl "text-overflow" (value_of v)
let word_break v = decl "word-break" (value_of v)
let overflow_wrap v = decl "overflow-wrap" (value_of v)
let list_style v = decl "list-style" (value_of v)
let list_style_type v = decl "list-style-type" (value_of v)
let list_style_position v = decl "list-style-position" (value_of v)
let cursor v = decl "cursor" (value_of v)
let pointer_events v = decl "pointer-events" (value_of v)
let user_select v = decl "user-select" (value_of v)
let visibility v = decl "visibility" (value_of v)
let border_style v = decl "border-style" (value_of v)
let border_collapse v = decl "border-collapse" (value_of v)
let background_repeat v = decl "background-repeat" (value_of v)
let background_size v = decl "background-size" (value_of v)
let background_image images = decl "background-image" (Comma (List.map image_or_value images))
let background v = decl "background" (image_or_value v)
let background_clip v = decl "background-clip" (value_of v)
let background_attachment v = decl "background-attachment" (value_of v)
let font_variant_numeric vs = decl "font-variant-numeric" (Space (List.map value_of vs))
let object_fit v = decl "object-fit" (value_of v)
let vertical_align v = decl "vertical-align" (value_of v)
let resize v = decl "resize" (value_of v)
let scroll_behavior v = decl "scroll-behavior" (value_of v)
let float_ v = decl "float" (value_of v)
let clear v = decl "clear" (value_of v)
let table_layout v = decl "table-layout" (value_of v)
let font_display v = decl "font-display" (value_of v)
let content v = decl "content" (value_of v)
let aspect_ratio v = decl "aspect-ratio" (value_of v)
let flex v = decl "flex" (value_of v)
let z_index v = decl "z-index" (value_of v)
let order n = decl "order" (Integer n)
let opacity f = decl "opacity" (Number f)
let flex_grow f = decl "flex-grow" (Number f)
let flex_shrink f = decl "flex-shrink" (Number f)
let border v = decl "border" (border_value v)
let border_top v = decl "border-top" (border_value v)
let border_right v = decl "border-right" (border_value v)
let border_bottom v = decl "border-bottom" (border_value v)
let border_left v = decl "border-left" (border_value v)
let outline v = decl "outline" (border_value v)
let box_shadow v = decl "box-shadow" (shadow_value v)
let font_family fs = decl "font-family" (Comma (List.map value_of fs))
let transition ts = decl "transition" (match ts with [] -> Keyword "none" | ts -> Comma ts)
let animation anims = decl "animation" (match anims with [] -> Keyword "none" | a -> Comma a)
let transform fs = decl "transform" (functions_or_none transform_value fs)
let filter fs = decl "filter" (functions_or_none filter_value fs)
let backdrop_filter fs = decl "backdrop-filter" (functions_or_none filter_value fs)
let clip_path shape = decl "clip-path" (clip_value shape)
let will_change props =
  decl "will-change"
    (match props with
     | [] -> Keyword "auto"
     | ps -> Comma (List.map (fun p -> Keyword (Transition.string_of_property p)) ps))
let grid_template_columns tracks = decl "grid-template-columns" (Space (List.map track_value tracks))
let grid_template_rows tracks = decl "grid-template-rows" (Space (List.map track_value tracks))
let grid_template_areas rows = decl "grid-template-areas" (Space (List.map (fun r -> String r) rows))
let grid_area name = decl "grid-area" (Keyword name)
let grid_column line = decl "grid-column" (grid_line_value line)
let grid_row line = decl "grid-row" (grid_line_value line)
let background_position x y = decl "background-position" (Space [position_value x; position_value y])
let transform_origin x y = decl "transform-origin" (Space [position_value x; position_value y])
let unicode_range ranges =
  decl "unicode-range"
    (Comma (List.map (fun (a, b) ->
       Keyword (if a = b then Printf.sprintf "U+%04X" a else Printf.sprintf "U+%04X-%04X" a b)) ranges))

let margin2 v h = decl "margin" (Space [value_of v; value_of h])
let margin4 t r b l = decl "margin" (Space [value_of t; value_of r; value_of b; value_of l])
let padding2 v h = decl "padding" (Space [value_of v; value_of h])
let padding4 t r b l = decl "padding" (Space [value_of t; value_of r; value_of b; value_of l])
