module H = Html
module D = Datastar
module DA = D.Attributes

let check name expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n" name expected actual;
    exit 1
  end

let render_attr a = H.render (H.div [a] [])
let sse ev = String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\n") ev)

let () =
  check "signal camelCase key" {|<div data-signals:user-name="&#39;x&#39;"></div>|}
    (render_attr (DA.signal "userName" "'x'"));
  check "signals object" {|<div data-signals__ifmissing="{&quot;count&quot;: 0}"></div>|}
    (render_attr (DA.signals ~ifmissing:true ["count", "0"]));
  check "on modifiers" {|<div data-on:click__prevent__debounce.300ms.leading="$x++"></div>|}
    (render_attr (DA.on ~mods:[`prevent; `debounce_with (`ms 300, [`leading])] "click" "$x++"));
  check "bind mods" {|<div data-bind__prop.checked__event.change="flag"></div>|}
    (render_attr (DA.bind ~prop:"checked" ~event:"change" "flag"));
  check "interval" {|<div data-on-interval__duration.2s.leading="@get(&quot;/tick&quot;)"></div>|}
    (render_attr (DA.on_interval ~duration:(`s 2) ~leading:true (D.Actions.get "/tick")));
  check "action options"
    {|@post("/save", {"contentType": "form", "openWhenHidden": true, "filterSignals": {"include": "^form\\."}})|}
    (D.Actions.post ~content_type:`form ~open_when_hidden:true ~include_:"^form\\." "/save");
  check "patch elements multiline"
    "event: datastar-patch-elements\ndata: selector #mails\ndata: mode inner\ndata: elements <li>a\ndata: elements b</li>\n"
    (sse (D.Sse.patch_elements ~selector:"#mails" ~mode:`inner [H.li [] [H.text "a\nb"]]));
  check "remove" "event: datastar-patch-elements\ndata: selector #x\ndata: mode remove\n"
    (sse (D.Sse.remove_elements "#x"));
  check "patch signals" "event: datastar-patch-signals\ndata: onlyIfMissing true\ndata: signals {\"a\":1}\n"
    (sse (D.Sse.patch_signals ~only_if_missing:true (`Assoc ["a", `Int 1])));
  check "execute script"
    "event: datastar-patch-elements\ndata: selector body\ndata: mode append\ndata: elements <script data-effect=\"el.remove()\">console.log(1 < 2)</script>\n"
    (sse (D.Sse.execute_script "console.log(1 < 2)"));
  print_endline "ok"
