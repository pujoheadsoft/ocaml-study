(* Deep Handler で実装した Ask エフェクト *)
open Effect
open Effect.Deep

module type ASK = sig
  type t

  val ask : unit -> t

  val run : (unit -> 'a) -> init:t -> 'a
end

module Ask (S : sig type t end) : ASK with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Ask : t Effect.t

  let ask () : t = perform Ask

  (*
    Deep Handlerを使って実装した run関数
    Shallowのときのようにループ処理は不要。
   *)
  let run (f: unit -> 'a) ~(init: t) : 'a =  
    match_with f ()  
    { retc = (fun result -> result);  
      exnc = (fun e -> raise e);  
      effc = (fun (type b) (eff: b Effect.t) ->  
        match eff with  
        | Ask -> Some (fun (k: (b,_) continuation) ->  
            continue k init)  
        | _ -> None)  
    }
end

module IntAsk = Ask (struct type t = int end)  
module StringAsk = Ask (struct type t = string end) 

let exampleIntAsk () =
  let value = IntAsk.ask () in
  Printf.printf "Got value: %d\n" value;
  let another = IntAsk.ask () in
  Printf.printf "Got same value: %d\n" another

let exampleStringAsk () =
  let value = StringAsk.ask () in
  Printf.printf "Got value: %s\n" value;
  let another = StringAsk.ask () in
  Printf.printf "Got same value: %s\n" another

let runExampleIntAsk () = IntAsk.run exampleIntAsk ~init:43

let runExampleStringAsk () = StringAsk.run exampleStringAsk ~init:"Hello"