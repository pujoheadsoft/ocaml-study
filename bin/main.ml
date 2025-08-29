open Effect_ask_simple
open Effect_emit_simple

let askEmit () =
  let value = Ask.ask () in
  Emit.emit value

let exampleAskEmit () =
  Ask.run (fun () -> Emit.run askEmit) ~env:"Hello World"

let () = exampleAskEmit ()

