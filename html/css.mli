(** CSS as an AST.

    A selector is a function from declarations to a rule, so a stylesheet
    is a list of selectors applied to declarations. Optionally run
    {!Optimize} passes over it, then print it with {!to_string}.

    {[
      let open Css in
      Selector.(
        [ (any > ul !li) [ padding (`px 5); color (`hex "#333") ]
        ; media "(max-width: 600px)" [ !body [ font_size (`rem 0.9) ] ] ])
      |> Optimize.all
      |> to_string
    ]}

    Opening [Selector] shadows [!], [>], [+] and [to_string]; keep it to
    the stylesheet expression, as above. *)

(** {1 Names}

    Class and id names are values, shared by stylesheets
    ({!Selector.cls}, {!Selector.id}) and HTML
    ([Html.Attributes.class_], [Html.Attributes.id]). *)

module Class : sig
  type t
  val make : string -> t
  val name : t -> string

  (** [".name"] *)
  val selector : t -> string
end

module Id : sig
  type t
  val make : string -> t
  val name : t -> string

  (** ["#name"], e.g. for [Datastar.Sse.patch_elements ~selector]. *)
  val selector : t -> string
end

(** {1 AST} *)

module Ast : sig
  (** Selectors *)

  type nth = [ `odd | `even | `n of int | `an_plus_b of int * int ]

  type attr_op =
    [ `exists                (** [a] *)
    | `equals of string      (** [a="v"] *)
    | `prefix of string      (** [a^="v"] *)
    | `suffix of string      (** [a$="v"] *)
    | `contains of string    (** [a*="v"] *)
    | `word of string        (** [a~="v"] *)
    | `dash of string ]      (** [a|="v"] *)

  (** [("id", `prefix "mail-")] is [[id^="mail-"]]. *)
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

  (** ['s] is the type of selector arguments ([:not(…)] and friends). *)
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

  (** A complex selector: compounds joined by combinators, left to right. *)
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

(** {1 Selectors}

    Element builders take the selector {e inside} them as their last
    argument, so juxtaposition reads like CSS: [ul !li] is [ul li].
    [!] ends a chain: [!li] is [li]. Classes, ids, attributes and pseudo
    classes are optional arguments: [!(li ~cls:["active"] ~pseudo:[`hover])]
    is [li.active:hover]. Deeper chains nest: [nav (ul !li)].

    A selector is applied to its declarations to make a rule:
    [(any > ul !li) [color `red]]. *)

module Selector : sig
  type nth = Ast.nth
  type attr_op = Ast.attr_op
  type attr_match = Ast.attr_match
  type pseudo_element = Ast.pseudo_element

  type t = Ast.declaration list -> Ast.stmt

  type pseudo_class = t Ast.pseudo

  type builder =
    ?id:Id.t -> ?cls:Class.t list -> ?attr:attr_match list ->
    ?pseudo:pseudo_class list -> ?pseudo_el:pseudo_element -> t -> t

  (** The empty selector, ending a chain. Printed as [*] if used alone. *)
  val nil : t
  val is_nil : t -> bool

  val any : t
  val root : t

  (** [group [a; b]] is [a, b]. Combinators distribute over groups:
      [group [a; b] > c] is [a > c, b > c]. *)
  val group : t list -> t

  (** The selectors in [t] (more than one for a group). *)
  val selectors : t -> Ast.selector list
  val of_selectors : Ast.selector list -> t

  (** Generic element / compound builders. *)
  val el :
    ?tag:string -> ?id:Id.t -> ?cls:Class.t list -> ?attr:attr_match list ->
    ?pseudo:pseudo_class list -> ?pseudo_el:pseudo_element -> t -> t
  val tag : string -> builder
  val cls : Class.t -> builder
  val id :
    Id.t -> ?cls:Class.t list -> ?attr:attr_match list ->
    ?pseudo:pseudo_class list -> ?pseudo_el:pseudo_element -> t -> t

  val html : builder
  val head : builder
  val body : builder
  val title : builder
  val style : builder
  val script : builder
  val noscript : builder
  val template : builder
  val main : builder
  val header : builder
  val footer : builder
  val nav : builder
  val section : builder
  val article : builder
  val aside : builder
  val address : builder
  val search : builder
  val h1 : builder
  val h2 : builder
  val h3 : builder
  val h4 : builder
  val h5 : builder
  val h6 : builder
  val hgroup : builder
  val div : builder
  val p : builder
  val pre : builder
  val blockquote : builder
  val figure : builder
  val figcaption : builder
  val hr : builder
  val ul : builder
  val menu : builder
  val ol : builder
  val li : builder
  val dl : builder
  val dt : builder
  val dd : builder
  val a : builder
  val span : builder
  val strong : builder
  val em : builder
  val b : builder
  val i : builder
  val u : builder
  val s : builder
  val small : builder
  val mark : builder
  val code : builder
  val kbd : builder
  val samp : builder
  val var : builder
  val sub : builder
  val sup : builder
  val abbr : builder
  val bdi : builder
  val bdo : builder
  val cite : builder
  val q : builder
  val time : builder
  val data : builder
  val del : builder
  val ins : builder
  val ruby : builder
  val rt : builder
  val rp : builder
  val br : builder
  val wbr : builder
  val form : builder
  val label : builder
  val button : builder
  val input : builder
  val select : builder
  val optgroup : builder
  val option : builder
  val textarea : builder
  val fieldset : builder
  val legend : builder
  val output : builder
  val datalist : builder
  val progress : builder
  val meter : builder
  val table : builder
  val caption : builder
  val colgroup : builder
  val col : builder
  val thead : builder
  val tbody : builder
  val tfoot : builder
  val tr : builder
  val th : builder
  val td : builder
  val img : builder
  val picture : builder
  val source : builder
  val video : builder
  val audio : builder
  val track : builder
  val iframe : builder
  val embed : builder
  val canvas : builder
  val svg : builder
  val map : builder
  val area : builder
  val details : builder
  val summary : builder
  val dialog : builder
  val slot : builder

  val descendant : t -> t -> t
  val child : t -> t -> t
  val next_sibling : t -> t -> t
  val subsequent_sibling : t -> t -> t

  val to_string : ?minify:bool -> t -> string
  val print : ?minify:bool -> Ast.selector -> string
  val print_list : ?minify:bool -> Ast.selector list -> string

  (** (ids, classes/attributes/pseudo-classes, types/pseudo-elements); the
      highest in a group. *)
  val specificity : t -> int * int * int

  (** CSS string literal with escaping. *)
  val quote : string -> string

  (** {2 Operators} *)

  (** [!li] is [li]: applies a builder to {!nil}. *)
  val ( ! ) : (t -> t) -> t

  (** [a > b]: child. *)
  val ( > ) : t -> t -> t

  (** [a + b]: next sibling. *)
  val ( + ) : t -> t -> t

  (** [a +~ b]: subsequent sibling ([a ~ b]). *)
  val ( +~ ) : t -> t -> t
end

type declaration = Ast.declaration
type stmt = Ast.stmt
type stylesheet = Ast.stylesheet

(** {1 Typed values}

    Keywords that are OCaml keywords get a trailing underscore
    ([`inherit_], [`end_]). Every property also accepts {!global}:
    [`var "name"] for [var(--name)] and [`raw "..."] as an escape hatch. *)

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

(** Color functions usable with relative color syntax ([oklch(from …)]). *)
type relative_space = [ `rgb | `hsl | `hwb | `lab | `lch | `oklab | `oklch ]

(** [Lab]/[Lch] lightness is a percentage (0–100); [Oklab]/[Oklch]
    lightness is 0–1. Hue is in degrees. The [*a] variants add alpha. *)
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
      (** [color-mix(in space, a p%, b)]; [p] is a percentage. *)
  | `from of relative_space * color * string * string * string * string option
      (** Relative color: [`from (`oklch, c, "l", "c", "calc(h + 30)", None)] is
          [oklch(from c l c calc(h + 30))]. *) ]

type font_weight = [ font_weight_kw | `w of int ]
type font_family = [ generic_family | `name of string ]
type number = [ `num of float ]

type time = [ `ms of float | `s of float ]

type easing =
  [ `linear | `ease | `ease_in | `ease_out | `ease_in_out | `step_start | `step_end
  | `cubic_bezier of float * float * float * float | `steps of int ]

type transform_fn =
  [ `translate of length * length | `translate_x of length | `translate_y of length
  | `scale of float | `scale_xy of float * float | `scale_x of float | `scale_y of float
  | `rotate of float                                        (** degrees *)
  | `skew_x of float | `skew_y of float                    (** degrees *)
  | `perspective of length ]

type filter_fn =
  [ `blur of length | `brightness of float | `contrast of float | `grayscale of float
  | `hue_rotate of float                                    (** degrees *)
  | `invert of float | `opacity of float | `saturate of float | `sepia of float
  | `drop_shadow of length * length * length * color ]

type track_size = [ length | `auto | `min_content | `max_content ]

(** A grid track: [`minmax (`zero, `fr 1.)], [`repeat (`auto_fill, [`minmax (`rem 10., `fr 1.)])]. *)
type track =
  [ length | `auto | `min_content | `max_content
  | `minmax of track_size * track_size
  | `fit_content of length
  | `repeat of [ `n of int | `auto_fill | `auto_fit ] * track list ]

(** [`between (1, 3)] is [1 / 3]; [`area "main"] a named area. *)
type grid_line =
  [ `auto | `line of int | `span of int | `between of int * int | `area of string ]

type position_xy = [ length | `left | `center | `right | `top | `bottom ]

type clip_shape =
  [ `none
  | `inset of length * length * length * length
  | `circle of length * (length * length)                   (** radius, at (x, y) *)
  | `polygon of (length * length) list
  | `url of string ]

(** A color stop: a color and an optional position. *)
type stop = color * length option

type radial_shape = [ `circle | `ellipse | `size of length * length ]

type image =
  [ `url of string
  | `linear of float * stop list                              (** angle in degrees *)
  | `radial of radial_shape * (length * length) * stop list ]  (** shape, at (x, y) *)

type shadow =
  [ `shadow of length * length * length * color                  (** x y blur color *)
  | `spread of length * length * length * length * color         (** x y blur spread color *)
  | `inner of length * length * length * length * color ]        (** inset x y blur spread color *)

(** Every non-nested value. *)
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

val value_of : [< simple ] -> Ast.value

(** {1 Color manipulation}

    Operations compute the result at build time when the input is a
    concrete color, returning OKLCH (which keeps wide-gamut colors). When it
    isn't (a [`var], [`current_color], a mix…), they fall back to CSS
    relative color syntax or [color-mix()], so the browser computes it:
    [Color.lighten 0.1 (`var "accent")] is
    [oklch(from var(--accent) calc(l + 0.1) c h)]. *)

module Color : sig
  type t = color

  (** {2 Conversions} ([None] if not computable at build time) *)

  (** OKLab [(l, a, b, alpha)]. *)
  val to_oklab : t -> (float * float * float * float) option

  (** OKLCH [(l, c, h, alpha)], hue in degrees. *)
  val to_oklch : t -> (float * float * float * float) option

  (** Gamma-encoded sRGB [(r, g, b, alpha)] in 0–1, unclamped. *)
  val to_srgb : t -> (float * float * float * float) option

  (** ["#rrggbb"] or ["#rrggbbaa"], clipped to the sRGB gamut. *)
  val to_hex : t -> string option

  (** [`hex] when computable, unchanged otherwise. *)
  val hex : t -> t

  (** Build an OKLCH color (values rounded to 4 decimals). *)
  val oklch : ?alpha:float -> float -> float -> float -> t

  (** Build an OKLab color (values rounded to 4 decimals). *)
  val oklab : ?alpha:float -> float -> float -> float -> t

  (** {2 Channels}

      Each channel takes an operation; omitted channels are kept. Static
      results stay in the same space; otherwise relative color syntax:
      [adjust_oklab ~a:(`add 0.05) (`var "x")] is
      [oklab(from var(--x) l calc(a + 0.05) b)]. Lightness and alpha are
      clamped to 0–1, chroma to ≥ 0, hue wraps to 0–360. *)

  type op = [ `set of float | `add of float | `mul of float ]

  (** [l] 0–1; [a] green(−)↔red(+); [b] blue(−)↔yellow(+). *)
  val adjust_oklab : ?l:op -> ?a:op -> ?b:op -> ?alpha:op -> t -> t

  (** [l] 0–1; [c] chroma (≈ 0–0.4); [h] hue in degrees. *)
  val adjust_oklch : ?l:op -> ?c:op -> ?h:op -> ?alpha:op -> t -> t

  (** {2 Adjustments} *)

  (** Add to OKLCH lightness (0–1 scale): [lighten 0.1]. *)
  val lighten : float -> t -> t
  val darken : float -> t -> t

  (** Scale chroma by [1 + amount]: [saturate 0.2] is 20% more chroma. *)
  val saturate : float -> t -> t
  val desaturate : float -> t -> t

  (** Rotate hue by degrees. *)
  val rotate : float -> t -> t
  val complement : t -> t
  val grayscale : t -> t
  val invert : t -> t

  (** Set alpha (0–1). *)
  val with_alpha : float -> t -> t

  (** Mix two colors; [weight] (default 0.5) is the proportion of the first.
      Computed statically for [`oklab] (default), [`oklch] and [`srgb];
      otherwise [color-mix()]. *)
  val mix : ?space:color_space -> ?weight:float -> t -> t -> t

  (** {2 Accessibility} *)

  (** WCAG relative luminance (0–1). *)
  val luminance : t -> float option

  (** WCAG contrast ratio (1–21). *)
  val contrast : t -> t -> float option

  (** Whichever of [light] (default white) and [dark] (default black)
      contrasts more with the background; [dark] if unknown. *)
  val readable_on : ?light:t -> ?dark:t -> t -> t
end

(** {1 Statements} *)

type media_feature =
  [ `screen | `print
  | `min_width of length | `max_width of length
  | `min_height of length | `max_height of length
  | `orientation of [ `portrait | `landscape ]
  | `prefers_color_scheme of [ `light | `dark ]
  | `prefers_reduced_motion of [ `reduce | `no_preference ]
  | `hover of [ `hover | `none ]
  | `pointer of [ `fine | `coarse | `none ] ]

(** [media [`screen; `max_width (`px 600)] [...]] is
    [@media screen and (max-width: 600px)]; all features must hold. *)
val media : media_feature list -> stmt list -> stmt

(** [supports [display `grid] [...]] is [@supports (display: grid)]. *)
val supports : declaration list -> stmt list -> stmt
val font_face : declaration list -> stmt
val keyframes : string -> ([< `from | `to_ | `at of float ] list * declaration list) list -> stmt

(** Named keyframes, referenced by {!Animation.make}:
    {[
      let pulse = Keyframes.make "pulse" [ [`from], [...]; [`to_], [...] ] in
      [ Keyframes.declare pulse
      ; !(cls dot) [animation [Animation.make ~duration:(`s 2.) ~iterations:`infinite pulse]] ]
    ]} *)
module Keyframes : sig
  type t
  val make : string -> ([< `from | `to_ | `at of float ] list * declaration list) list -> t
  val name : t -> string
  val declare : t -> stmt
end

(** One transition, for {!transition}. Defaults: 150ms, [`ease]. *)
module Transition : sig
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

  val string_of_property : property -> string

  type t
  val make : ?duration:time -> ?easing:easing -> ?delay:time -> property -> t
end

(** One animation, for {!animation}. Defaults: 300ms, [`ease]. *)
module Animation : sig
  type t
  val make :
    ?duration:time -> ?easing:easing -> ?delay:time ->
    ?iterations:[ `infinite | `times of float ] ->
    ?direction:[ `normal | `reverse | `alternate | `alternate_reverse ] ->
    ?fill:[ `none | `forwards | `backwards | `both ] ->
    ?play_state:[ `running | `paused ] ->
    Keyframes.t -> t
end
val import : ?media:string -> string -> stmt
val raw : string -> stmt

(** {1 Declarations} *)

val decl : string -> Ast.value -> declaration
val important : declaration -> declaration

(** Untyped declaration: [prop "key" "value"]. *)
val prop : string -> string -> declaration

(** Custom property: [custom "accent" (`hex "#f00")] is [--accent: #f00]. *)
val custom : string -> [< simple ] -> declaration

(** Custom properties as values, so they're referenced by OCaml name:
    {[
      let text = Var.make "text" (`hex "#eee") in
      [ root (Var.declarations [text])          (* --text: #eee *)
      ; !body [color (Var.use text)] ]          (* color: var(--text) *)
    ]}
    {!undefined_vars} catches a variable that's used but never declared. *)
module Var : sig
  type t
  val make : string -> [< simple ] -> t
  val name : t -> string

  (** [var(--name)], usable wherever a value is. *)
  val use : t -> [> `var of string ]

  (** [--name: value]. *)
  val declare : t -> declaration
  val declarations : t list -> declaration list
end

(** [src] descriptor for {!font_face}. *)
val font_src : [< `url of string | `url_format of string * string | `local of string ] list -> declaration

(** {1 Printing} *)

val to_string : ?minify:bool -> stylesheet -> string
val value_to_string : ?minify:bool -> Ast.value -> string
val declaration_to_string : ?minify:bool -> declaration -> string

(** For [style] attributes: [a: b; c: d]. *)
val inline : declaration list -> string

(** Custom properties referenced with [var(--x)] (including in fallbacks
    and color functions) that no declaration in the stylesheet defines.
    Can't see inside [Raw] values or inline [style] attributes. *)
val undefined_vars : stylesheet -> string list

(** {1 Optimization passes}

    All are [stylesheet -> stylesheet] and recurse into [@media] and
    [@supports]. They preserve the cascade. *)

module Optimize : sig
  val map_blocks : (stmt list -> stmt list) -> stylesheet -> stylesheet
  val map_declarations : (declaration list -> declaration list) -> stylesheet -> stylesheet

  (** Drops rules, at-rules and blocks with nothing in them. *)
  val remove_empty : stylesheet -> stylesheet

  (** [0px] → [0], [#AABBCC] → [#abc], [1.0] → [1]. *)
  val minify_values : stylesheet -> stylesheet

  (** Keeps only the winning declaration per property, honouring
      [!important]. Note: drops intentional fallbacks such as
      [display: -webkit-box; display: flex]. *)
  val dedupe_declarations : stylesheet -> stylesheet

  val dedupe_selectors : stylesheet -> stylesheet

  (** [a {x} … a {y}] → [a {x; y}] …: merges each rule into the closest
      earlier rule with the same selectors, when moving its declarations up
      can't change the cascade: nothing in between sets a related property
      ([margin] vs [margin-top], …) with the same importance, at equal
      specificity, from a selector that could match the same element
      ([a] and [b], [#x] and [#y], [:enabled] and [:disabled], [.x] and
      [:not(.x)] never do). Raw CSS in between blocks merging. *)
  val merge_same_selectors : stylesheet -> stylesheet

  (** [a {x} … b {x}] → [a, b {x}] …, under the same conditions as
      {!merge_same_selectors}. *)
  val merge_same_declarations : stylesheet -> stylesheet

  (** All of the above. *)
  val all : stylesheet -> stylesheet
end

(** {1 Properties} *)

val color : [< color | global ] -> declaration
val background_color : [< color | global ] -> declaration
val border_color : [< color | global ] -> declaration
val border_top_color : [< color | global ] -> declaration
val border_right_color : [< color | global ] -> declaration
val border_bottom_color : [< color | global ] -> declaration
val border_left_color : [< color | global ] -> declaration
val outline_color : [< color | global ] -> declaration
val accent_color : [< color | global ] -> declaration
val caret_color : [< color | global ] -> declaration
val text_decoration_color : [< color | global ] -> declaration
val fill : [< color | global ] -> declaration
val stroke : [< color | global ] -> declaration
val width : [< length | `auto | global ] -> declaration
val height : [< length | `auto | global ] -> declaration
val min_width : [< length | `auto | global ] -> declaration
val min_height : [< length | `auto | global ] -> declaration
val top : [< length | `auto | global ] -> declaration
val right : [< length | `auto | global ] -> declaration
val bottom : [< length | `auto | global ] -> declaration
val left : [< length | `auto | global ] -> declaration
val inset : [< length | `auto | global ] -> declaration
val max_width : [< length | `none | global ] -> declaration
val max_height : [< length | `none | global ] -> declaration
val margin : [< length | `auto | global ] -> declaration
val margin_top : [< length | `auto | global ] -> declaration
val margin_right : [< length | `auto | global ] -> declaration
val margin_bottom : [< length | `auto | global ] -> declaration
val margin_left : [< length | `auto | global ] -> declaration
val margin_inline : [< length | `auto | global ] -> declaration
val margin_block : [< length | `auto | global ] -> declaration
val margin_block_start : [< length | `auto | global ] -> declaration
val margin_block_end : [< length | `auto | global ] -> declaration
val margin_inline_start : [< length | `auto | global ] -> declaration
val margin_inline_end : [< length | `auto | global ] -> declaration
val padding : [< length | global ] -> declaration
val padding_top : [< length | global ] -> declaration
val padding_right : [< length | global ] -> declaration
val padding_bottom : [< length | global ] -> declaration
val padding_left : [< length | global ] -> declaration
val padding_inline : [< length | global ] -> declaration
val padding_block : [< length | global ] -> declaration
val padding_block_start : [< length | global ] -> declaration
val padding_block_end : [< length | global ] -> declaration
val padding_inline_start : [< length | global ] -> declaration
val padding_inline_end : [< length | global ] -> declaration
val gap : [< length | global ] -> declaration
val row_gap : [< length | global ] -> declaration
val column_gap : [< length | global ] -> declaration
val border_radius : [< length | global ] -> declaration
val outline_offset : [< length | global ] -> declaration
val text_indent : [< length | global ] -> declaration
val letter_spacing : [< length | `normal | global ] -> declaration
val word_spacing : [< length | `normal | global ] -> declaration
val border_width : [< length | border_width_kw | global ] -> declaration
val outline_width : [< length | border_width_kw | global ] -> declaration
val flex_basis : [< length | `auto | `content | global ] -> declaration
val font_size : [< length | font_size_kw | global ] -> declaration
val line_height : [< length | `normal | number | global ] -> declaration
val font_weight : [< font_weight | global ] -> declaration
val font_style : [< font_style | global ] -> declaration
val display : [< display | global ] -> declaration
val position : [< position | global ] -> declaration
val box_sizing : [< box_sizing | global ] -> declaration
val overflow : [< overflow | global ] -> declaration
val overflow_x : [< overflow | global ] -> declaration
val overflow_y : [< overflow | global ] -> declaration
val flex_direction : [< flex_direction | global ] -> declaration
val flex_wrap : [< flex_wrap | global ] -> declaration
val justify_content : [< alignment | global ] -> declaration
val justify_items : [< alignment | global ] -> declaration
val justify_self : [< alignment | global ] -> declaration
val align_items : [< alignment | global ] -> declaration
val align_self : [< alignment | global ] -> declaration
val align_content : [< alignment | global ] -> declaration
val place_items : [< alignment | global ] -> declaration
val place_content : [< alignment | global ] -> declaration
val place_self : [< alignment | global ] -> declaration
val text_align : [< text_align | global ] -> declaration
val text_decoration : [< text_decoration | global ] -> declaration
val text_decoration_line : [< text_decoration | global ] -> declaration
val text_transform : [< text_transform | global ] -> declaration
val white_space : [< white_space | global ] -> declaration
val text_overflow : [< text_overflow | global ] -> declaration
val word_break : [< word_break | global ] -> declaration
val overflow_wrap : [< overflow_wrap | global ] -> declaration
val list_style : [< list_style_type | global ] -> declaration
val list_style_type : [< list_style_type | global ] -> declaration
val list_style_position : [< list_style_position | global ] -> declaration
val cursor : [< cursor | global ] -> declaration
val pointer_events : [< pointer_events | global ] -> declaration
val user_select : [< user_select | global ] -> declaration
val visibility : [< visibility | global ] -> declaration
val border_style : [< border_style | global ] -> declaration
val border_collapse : [< border_collapse | global ] -> declaration
val background_repeat : [< background_repeat | global ] -> declaration
val background_size : [< length | background_size_kw | global ] -> declaration
(** Layers, first on top: [background_image [`linear (135., [c1, None; c2, Some (`percent 60.)])]]. *)
val background_image : [< image | `none | global ] list -> declaration
val background : [< color | image | `none | global ] -> declaration
val background_clip : [< background_clip | global ] -> declaration
val background_attachment : [< background_attachment | global ] -> declaration
val font_variant_numeric : [< font_variant_numeric | global ] list -> declaration
val object_fit : [< object_fit | global ] -> declaration
val vertical_align : [< vertical_align | length | global ] -> declaration
val resize : [< resize | global ] -> declaration
val scroll_behavior : [< scroll_behavior | global ] -> declaration
val float_ : [< float_kw | global ] -> declaration
val clear : [< clear | global ] -> declaration
val table_layout : [< table_layout | global ] -> declaration
val font_display : [< font_display | global ] -> declaration
val content : [< `none | `normal | `string of string | `attr of string | global ] -> declaration
val aspect_ratio : [< `auto | `ratio of int * int | global ] -> declaration
val flex : [< `none | `auto | number | global ] -> declaration
val z_index : [< `auto | `int of int | global ] -> declaration
val order : int -> declaration
val opacity : float -> declaration
val flex_grow : float -> declaration
val flex_shrink : float -> declaration
val border : [< `none | `border of length * border_style * color | global ] -> declaration
val border_top : [< `none | `border of length * border_style * color | global ] -> declaration
val border_right : [< `none | `border of length * border_style * color | global ] -> declaration
val border_bottom : [< `none | `border of length * border_style * color | global ] -> declaration
val border_left : [< `none | `border of length * border_style * color | global ] -> declaration
val outline : [< `none | `border of length * border_style * color | global ] -> declaration
val box_shadow : [< `none | shadow | global ] -> declaration
val font_family : [< font_family | global ] list -> declaration
(** [[]] is [none]. *)
val transition : Transition.t list -> declaration
val animation : Animation.t list -> declaration

(** [transform [`translate_y (`px (-2)); `scale 1.02]]; [[]] is [none]. *)
val transform : transform_fn list -> declaration
val filter : filter_fn list -> declaration
val backdrop_filter : filter_fn list -> declaration
val clip_path : clip_shape -> declaration

(** [[]] is [auto]. *)
val will_change : Transition.property list -> declaration

val grid_template_columns : track list -> declaration
val grid_template_rows : track list -> declaration

(** One string per row: [["header header"; "nav main"]]. *)
val grid_template_areas : string list -> declaration
val grid_area : string -> declaration
val grid_column : grid_line -> declaration
val grid_row : grid_line -> declaration

(** x, then y. *)
val background_position : position_xy -> position_xy -> declaration
val transform_origin : position_xy -> position_xy -> declaration

(** Code point ranges: [[(0x0000, 0x00FF); (0x2000, 0x206F)]]. *)
val unicode_range : (int * int) list -> declaration

(** Two- and four-value shorthands, in CSS order. *)

val margin2 : [< length | `auto | global ] -> [< length | `auto | global ] -> declaration
val margin4 :
  [< length | `auto | global ] -> [< length | `auto | global ] ->
  [< length | `auto | global ] -> [< length | `auto | global ] -> declaration
val padding2 : [< length | global ] -> [< length | global ] -> declaration
val padding4 :
  [< length | global ] -> [< length | global ] ->
  [< length | global ] -> [< length | global ] -> declaration
