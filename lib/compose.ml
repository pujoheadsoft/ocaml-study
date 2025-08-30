open Effects.Ask_deep
open Effects.Emit
open Printf

let ask_emit () =
  printf("Start\n");
  let value = StringAsk.ask() in
  StringEmit.emit value;
  printf("End\n")

let example_ask_emit () =
  StringAsk.run (fun () ->
    StringEmit.run ask_emit (fun value -> Printf.printf "Emit %s\n" value)
  ) ~env:"Ask Value"
