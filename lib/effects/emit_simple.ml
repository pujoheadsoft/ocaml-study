(*
  Stringに特化したシンプルなEmit
  ファンクターを使わないのでシンプル
*)
open Effect
open Effect.Deep

module Emit = struct
  type _ Effect.t += Emit : string -> unit Effect.t

  let emit (value: string) : unit = perform (Emit value)

  let run (f: unit -> 'a) : 'a =
    match_with f ()
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff: b Effect.t) ->
        match eff with
        | Emit value -> Some (fun (k: (b, _) continuation) ->
            Printf.printf "Emitted: %s\n" value;
            continue k ())
        | _ -> None)
    }

end

let exampleStringEmit () =
  Emit.emit "Hello World"

let runExampleStringEmit () = Emit.run exampleStringEmit