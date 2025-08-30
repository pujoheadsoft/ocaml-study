(*
  Deep Handler で実装した Ask エフェクト
  色々な型に対応できる
  文字列特化のシンプルなやつは effect_ask_simple.ml を参照

  細かい補足説明は effect_ask_shallow.ml を参照
*)
open Effect
open Effect.Deep

module type ASK = sig
  type t

  val ask : unit -> t

  val run : (unit -> 'a) -> env:t -> 'a
end

module Ask (S : sig type t end) : ASK with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Ask : t Effect.t

  let ask () : t = perform Ask

  (*
    Deep Handlerを使って実装した run関数
    Shallowのときのようにループ処理は不要。
   *)
  let run (f: unit -> 'a) ~(env: t) : 'a =  
    match_with f ()  
    { retc = Fun.id;  
      exnc = raise; 
      effc = (fun (type b) (eff: b Effect.t) ->  
        match eff with  
        | Ask -> Some (fun (k: (b,_) continuation) ->  
            continue k env)  
        | _ -> None)  
    }
end

module IntAsk = Ask (struct type t = int end)  
module StringAsk = Ask (struct type t = string end) 

let two_int_ask () =
  let value = IntAsk.ask () in
  Printf.printf "Got value: %d\n" value;
  let another = IntAsk.ask () in
  Printf.printf "Got same value: %d\n" another

let two_string_ask () =
  let value = StringAsk.ask () in
  Printf.printf "Got value: %s\n" value;
  let another = StringAsk.ask () in
  Printf.printf "Got same value: %s\n" another

let example_int_ask () = IntAsk.run two_int_ask ~env:43

let example_string_ask () = StringAsk.run two_string_ask ~env:"Hello"