(*
  Stringに特化したシンプルなAsk
  ファンクターを使わないのでシンプル
*)
open Effect
open Effect.Deep

module Ask = struct
  type _ Effect.t += Ask : string Effect.t

  let ask () : string = perform Ask

  let run (f: unit -> 'a) ~(env: string) : 'a =  
    match_with f ()  
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff: b Effect.t) ->
        match eff with
        | Ask -> Some (fun (k: (b, _) continuation) ->
            continue k env)
        | _ -> None)
    }
end

let exampleAsk () =
  let value = Ask.ask () in
  Printf.printf "Got value: %s\n" value;
  let another = Ask.ask () in
  Printf.printf "Got same value: %s\n" another

let runExampleStringAsk () = Ask.run exampleAsk ~env:"Hello"