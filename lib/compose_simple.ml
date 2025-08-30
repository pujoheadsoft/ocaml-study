open Effects.Ask_simple
open Effects.Emit_simple

let askEmit () =
  let value = Ask.ask () in
  Emit.emit value

let exampleAskEmitSimple () =
  Ask.run (fun () -> Emit.run askEmit) ~env:"Hello World"

