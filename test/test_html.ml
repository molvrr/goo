module H = Html

let check name expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n" name expected actual;
    exit 1
  end

let render_attr a = H.render (H.div [a] [])

let () =
  (* Css *)
  let open Css in
  let c = Class.make and idn = Id.make in
  check "selector juxtaposition" "* > ul li" (Selector.to_string Selector.(any > ul !li));
  check "selector compound" "nav#main ul li.active:hover"
    (Selector.to_string Selector.(nav ~id:(idn "main") (ul !(li ~cls:[c "active"] ~pseudo:[`hover]))));
  check "selector siblings" "li + li:nth-child(2n+1) ~ .x::before"
    (Selector.to_string Selector.(!li + !(li ~pseudo:[`nth_child (`an_plus_b (2, 1))]) +~ !(cls (c "x") ~pseudo_el:`before)));
  check "selector attr" {|li[id^="mail-"]:nth-child(odd)|}
    (Selector.to_string Selector.(!(li ~attr:["id", `prefix "mail-"] ~pseudo:[`nth_child `odd])));
  check "specificity" "1,2,3"
    (let (a, b, c) = Selector.(specificity (nav ~id:(idn "main") (ul !(li ~cls:[c "active"] ~pseudo:[`hover])))) in
     Printf.sprintf "%d,%d,%d" a b c);
  check "pretty" "div {\n  padding: 0px;\n  color: #AABBCC;\n}\n"
    (to_string [Selector.(!div) [padding (`px 0); color (`hex "#AABBCC")]]);
  check "optimize"
    "div{padding:0;color:#123;margin:0 auto}p,span{display:none!important}@media (max-width: 600px){body{line-height:1.5}}"
    (to_string ~minify:true (Optimize.all
      [ Selector.(!div) [padding (`px 0); color (`hex "#AABBCC"); color (`hex "#112233")]
      ; Selector.(!div) [margin2 (`px 0) `auto]
      ; Selector.(!p) [important (display `none); display `block]
      ; Selector.(!span) [important (display `none)]
      ; media [`max_width (`px 600)] [Selector.(!body) [line_height (`num 1.5)]; Selector.(!a) []]
      ; media [`print] [] ]));
  check "nested values" "border: 1px solid var(--accent); box-shadow: 0 1px 2px rgba(0, 0, 0, 0.2)"
    (inline [border (`border (`px 1, `solid, `var "accent")); box_shadow (`shadow (`zero, `px 1, `px 2, `rgba (0, 0, 0, 0.2)))]);
  check "style attribute" {|<div style="font-family: &quot;Inter&quot;, sans-serif"></div>|}
    (H.render (H.div [H.Attributes.style [font_family [`name "Inter"; `sans_serif]]] []));
  check "modern colors"
    "color: oklch(0.7 0.15 250); color: oklch(0.7 0.15 250 / 0.5); color: lch(50% 30 120); color: lab(50% 40 -20); color: oklab(0.6 -0.1 0.05); color: hwb(200 10% 20%); color: hsla(10, 20%, 30%, 0.4)"
    (inline [ color (`oklch (0.7, 0.15, 250.)); color (`oklcha (0.7, 0.15, 250., 0.5))
            ; color (`lch (50., 30., 120.)); color (`lab (50., 40., -20.))
            ; color (`oklab (0.6, -0.1, 0.05)); color (`hwb (200., 10., 20.)); color (`hsla (10, 20, 30, 0.4)) ]);
  let cv c = value_to_string (value_of c) in
  let opt = Option.value ~default:"None" in
  check "to_oklch" "0.6280 0.2577 29.2339"
    (opt (Option.map (fun (l, c, h, _) -> Printf.sprintf "%.4f %.4f %.4f" l c h) (Color.to_oklch (`hex "#ff0000"))));
  check "oklch to hex" "#ff0000" (opt (Color.to_hex (`oklch (0.628, 0.2577, 29.2339))));
  check "lab to hex" "#ff0000" (opt (Color.to_hex (`lab (54.29, 80.80, 69.89))));
  check "hsl to hex" "#008000" (opt (Color.to_hex (`hsl (120, 100, 25))));
  check "lighten static" "oklch(0.5993 0.0987 250.4331)" (cv (Color.lighten 0.1 (`hex "#336699")));
  check "lighten var" "oklch(from var(--accent) calc(l + 0.1) c h)" (cv (Color.lighten 0.1 (`var "accent")));
  check "rotate var" "oklch(from currentcolor l c calc(h + 30))" (cv (Color.rotate 30. `current_color));
  check "alpha var" "oklch(from var(--accent) l c h / 0.5)" (cv (Color.with_alpha 0.5 (`var "accent")));
  check "mix srgb" "#808080" (opt (Color.to_hex (Color.mix ~space:`srgb `black `white)));
  check "mix var" "color-mix(in oklab, var(--a) 30%, white)" (cv (Color.mix ~weight:0.3 (`var "a") `white));
  check "contrast" "4.54" (opt (Option.map (Printf.sprintf "%.2f") (Color.contrast (`hex "#767676") `white)));
  check "readable_on" "white" (cv (Color.readable_on (`hex "#336699")));
  check "adjust_oklab static" "oklab(0.628 0.2749 0.1258)"
    (cv (Color.adjust_oklab ~a:(`add 0.05) (`hex "#ff0000")));
  check "adjust_oklab var" "oklab(from var(--x) l calc(a + 0.05) calc(b * 0.5) / 0.8)"
    (cv (Color.adjust_oklab ~a:(`add 0.05) ~b:(`mul 0.5) ~alpha:(`set 0.8) (`var "x")));
  check "adjust_oklch var" "oklch(from var(--x) calc(l - 0.1) c 200)"
    (cv (Color.adjust_oklch ~l:(`add (-0.1)) ~h:(`set 200.) (`var "x")));
  check "selector applied" "nav ul li {\n  color: red;\n}\n"
    (let selector = Selector.(nav (ul !li)) in to_string [selector [color `red]]);
  check "group distributes" "ul > li, ol > li"
    (Selector.(to_string (group [!ul; !ol] > !li)));
  check "group rule" "h1,h2{margin:0}"
    (to_string ~minify:true [Selector.(group [!h1; !h2]) [margin `zero]]);
  let merged sheet = to_string ~minify:true (Optimize.merge_same_selectors sheet) in
  check "merge non-adjacent" "a{color:red;padding:0}b{margin:0}"
    (merged Selector.([ !a [color `red]; !b [margin `zero]; !a [padding `zero] ]));
  check "merge across disjoint selectors" "a{color:red;color:green}b{color:blue}"
    (merged Selector.([ !a [color `red]; !b [color `blue]; !a [color `green] ]));
  check "merge blocked by overlapping selectors" ".x{color:red}.y{color:blue}.x{color:green}"
    (merged Selector.([ !(cls (c "x")) [color `red]; !(cls (c "y")) [color `blue]; !(cls (c "x")) [color `green] ]));
  check "merge across different specificity" "a{color:red;color:green}.x{color:blue}"
    (merged Selector.([ !a [color `red]; !(cls (c "x")) [color `blue]; !a [color `green] ]));
  check "merge blocked by shorthand" ".x{margin:0}.y{margin-top:1px}.x{margin:2px}"
    (merged Selector.([ !(cls (c "x")) [margin `zero]; !(cls (c "y")) [margin_top (`px 1)]; !(cls (c "x")) [margin (`px 2)] ]));
  check "merge blocked by media" ".x{color:red}@media print{.y{color:blue}}.x{color:green}"
    (merged Selector.([ !(cls (c "x")) [color `red]; media [`print] [ !(cls (c "y")) [color `blue] ]; !(cls (c "x")) [color `green] ]));
  check "disjoint ids" "#a{color:red;color:green}#b{color:blue}"
    (merged Selector.([ !(id (idn "a")) [color `red]; !(id (idn "b")) [color `blue]; !(id (idn "a")) [color `green] ]));
  check "disjoint pseudo" "input:enabled{color:red;color:green}input:disabled{color:blue}"
    (merged Selector.([ !(input ~pseudo:[`enabled]) [color `red]; !(input ~pseudo:[`disabled]) [color `blue]
                      ; !(input ~pseudo:[`enabled]) [color `green] ]));
  check "disjoint not" "a.x{color:red;color:green}a:not(.x){color:blue}"
    (merged Selector.([ !(a ~cls:[c "x"]) [color `red]; !(a ~pseudo:[`not [!(cls (c "x"))]]) [color `blue]
                      ; !(a ~cls:[c "x"]) [color `green] ]));
  check "disjoint subject only" "ul li{color:red}ol li{color:blue}ul li{color:green}"
    (merged Selector.([ ul !li [color `red]; ol !li [color `blue]; ul !li [color `green] ]));
  let grouped sheet = to_string ~minify:true (Optimize.merge_same_declarations sheet) in
  check "group non-adjacent" ".x,.z{color:red}.y{margin:0}"
    (grouped Selector.([ !(cls (c "x")) [color `red]; !(cls (c "y")) [margin `zero]; !(cls (c "z")) [color `red] ]));
  check "group blocked" ".x{color:red}.y{color:blue}.z{color:red}"
    (grouped Selector.([ !(cls (c "x")) [color `red]; !(cls (c "y")) [color `blue]; !(cls (c "z")) [color `red] ]));
  check "group across disjoint" "a,p{color:red}b{color:blue}"
    (grouped Selector.([ !a [color `red]; !b [color `blue]; !p [color `red] ]));
  check "merge across unrelated important" "a{color:red;color:green}b{color:blue!important}"
    (merged Selector.([ !a [color `red]; !b [important (color `blue)]; !a [color `green] ]));
  check "gradients"
    "background-image: linear-gradient(135deg, red, blue 60%), radial-gradient(60rem 40rem at 10% -10%, oklch(0.7 0.1 200 / 0.3), transparent 60%)"
    (inline [background_image
      [ `linear (135., [`red, None; `blue, Some (`percent 60.)])
      ; `radial (`size (`rem 60., `rem 40.), (`percent 10., `percent (-10.)),
                 [`oklcha (0.7, 0.1, 200., 0.3), None; `transparent, Some (`percent 60.)]) ]]);
  check "radial circle" "background: radial-gradient(circle at 50% 50%, white, black)"
    (inline [background (`radial (`circle, (`percent 50., `percent 50.), [`white, None; `black, None]))]);
  check "zero angle keeps unit" "a{background:linear-gradient(0deg,red,blue)}"
    (to_string ~minify:true (Optimize.all Selector.([ !a [background (`linear (0., [`red, None; `blue, None]))] ])));
  check "shadows"
    "box-shadow: 0 0 0 8px transparent; box-shadow: inset 0 1px 2px 0 black; box-shadow: 0 1px 2px red"
    (inline [ box_shadow (`spread (`zero, `zero, `zero, `px 8, `transparent))
            ; box_shadow (`inner (`zero, `px 1, `px 2, `zero, `black))
            ; box_shadow (`shadow (`zero, `px 1, `px 2, `red)) ]);
  check "clip, attachment, numeric"
    "background-clip: text; background-attachment: fixed; font-variant-numeric: tabular-nums slashed-zero"
    (inline [ background_clip `text; background_attachment `fixed
            ; font_variant_numeric [`tabular_nums; `slashed_zero] ]);
  let text = Var.make "text" (`hex "#eee") in
  check "var declare/use" ":root{--text:#eee}body{color:var(--text)}"
    (to_string ~minify:true Selector.([ root (Var.declarations [text]); !body [color (Var.use text)] ]));
  let ghost = Var.make "ghost" `red in
  check "undefined vars" "ghost, missing"
    (String.concat ", " (undefined_vars Selector.(
       [ root (Var.declarations [text])
       ; !body [color (Var.use text); background_color (Var.use ghost)]
       ; !a [color (Color.lighten 0.1 (`var "missing"))] ])));
  let pulse = Keyframes.make "pulse" [[`from], [opacity 0.]; [`to_], [opacity 1.]] in
  check "typed motion"
    "transition: transform 120ms ease, background-color 150ms ease-out 50ms; animation: 2s ease-out infinite pulse; transform: translateY(-2px) scale(1.02) rotate(45deg)"
    (inline [ transition [ Transition.make ~duration:(`ms 120.) `transform
                         ; Transition.make ~easing:`ease_out ~delay:(`ms 50.) `background_color ]
            ; animation [Animation.make ~duration:(`s 2.) ~easing:`ease_out ~iterations:`infinite pulse]
            ; transform [`translate_y (`px (-2)); `scale 1.02; `rotate 45.] ]);
  check "keyframes handle" "@keyframes pulse{from{opacity:0}to{opacity:1}}"
    (to_string ~minify:true [Keyframes.declare pulse]);
  check "none lists" "transition: none; transform: none; filter: none; will-change: auto"
    (inline [transition []; transform []; filter []; will_change []]);
  check "filters" "backdrop-filter: blur(12px) saturate(1.5) hue-rotate(90deg)"
    (inline [backdrop_filter [`blur (`px 12); `saturate 1.5; `hue_rotate 90.]]);
  check "grid"
    "grid-template-columns: 2.75rem minmax(0, 1fr) auto; grid-template-rows: repeat(auto-fill, minmax(10rem, 1fr)); grid-template-areas: \"head head\" \"nav main\"; grid-column: 1 / 3; grid-row: span 2"
    (inline [ grid_template_columns [`rem 2.75; `minmax (`zero, `fr 1.); `auto]
            ; grid_template_rows [`repeat (`auto_fill, [`minmax (`rem 10., `fr 1.)])]
            ; grid_template_areas ["head head"; "nav main"]
            ; grid_column (`between (1, 3)); grid_row (`span 2) ]);
  check "positions, clip, unicode"
    "background-position: center top; transform-origin: 50% 0; clip-path: circle(50% at 50% 50%); unicode-range: U+0000-00FF, U+20AC; will-change: transform, opacity"
    (inline [ background_position `center `top; transform_origin (`percent 50.) `zero
            ; clip_path (`circle (`percent 50., (`percent 50., `percent 50.)))
            ; unicode_range [(0, 0xFF); (0x20AC, 0x20AC)]; will_change [`transform; `opacity] ]);
  check "media and supports"
    "@media screen and (max-width: 600px) and (prefers-reduced-motion: reduce){a{transition:none}}@supports (display: grid){a{display:grid}}"
    (to_string ~minify:true Selector.(
       [ media [`screen; `max_width (`px 600); `prefers_reduced_motion `reduce] [ !a [transition []] ]
       ; supports [display `grid] [ !a [display `grid] ] ]));
  check "class/id handles in html" {|<div id="x" class="a b"></div>|}
    (H.render (H.div [H.Attributes.id (idn "x"); H.Attributes.classes [c "a"; c "b"]] []));
  check "zero time keeps unit" "a{transition:opacity 0s ease}"
    (to_string ~minify:true (Optimize.all Selector.([ !a [transition [Transition.make ~duration:(`s 0.) `opacity]] ])));
  check "logical margins" "margin-block-start: 1rem; padding-inline-end: 0; transition: margin-block-start 150ms ease"
    (inline [ margin_block_start (`rem 1.); padding_inline_end `zero
            ; transition [Transition.make `margin_block_start] ]);
  print_endline "ok"
