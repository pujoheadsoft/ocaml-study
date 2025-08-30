open Effects.Ask_simple
open Effects.Emit_simple

let ask_emit () =
  let value = ask () in
  emit value

let example_ask_emit_simple () =
  run_ask (fun () -> run_emit ask_emit) ~env:"Hello World"

