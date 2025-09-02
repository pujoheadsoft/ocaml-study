open Lib
open Effects

let () =
  Printf.printf "Hello World\n";
  Compose.example_ask_emit ();
  Compose_simple.example_ask_emit_simple ();
  State.exec_example ();