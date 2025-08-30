open Effects.Ask_deep
open Effects.Emit

let askEmit () =
  let value = StringAsk.ask() in
  StringEmit.emit value

let exampleAskEmit () =
  StringAsk.run (fun () -> 
    StringEmit.run askEmit (fun value -> Printf.printf "Emitted: %s\n" value)
  ) ~env:"Hello World"

