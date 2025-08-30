open Effects.Ask_simple
open Effects.Emit_simple
open Printf

let ask_emit () =
  printf("Start\n");
  let value = ask () in
  emit value;
  printf("End\n")

let example_ask_emit_simple () =
  run_ask (fun () -> run_emit ask_emit) ~env:"Ask Value"

let example_ask_emit_simple2 () =
  run_emit (fun () -> run_ask ask_emit ~env:"Ask Value")