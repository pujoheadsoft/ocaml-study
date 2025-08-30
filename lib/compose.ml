open Effects.Ask_deep
open Effects.Emit

let ask_emit () =
  let value = StringAsk.ask() in
  StringEmit.emit value

let example_ask_emit () =
  StringAsk.run (fun () ->
    StringEmit.run ask_emit (fun value -> Printf.printf "Emitted: %s\n" value)
  ) ~env:"Hello World"
