(*
  Stringに特化したシンプルなAsk
  ファンクターを使わないのでシンプル
*)
open Effect
open Effect.Deep

type _ Effect.t += Ask : string Effect.t

let ask () : string = perform Ask

let run_ask (f: unit -> 'a) ~(env: string) : 'a =  
  match_with f ()  
  { retc = Fun.id;
    exnc = raise;
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Ask -> Some (fun (k: (b, _) continuation) ->
          continue k env)
      | _ -> None)
  }

let two_ask () =
  let value = ask () in
  Printf.printf "Got value: %s\n" value;
  let another = ask () in
  Printf.printf "Got same value: %s\n" another

let example_ask () = run_ask two_ask ~env:"Hello"