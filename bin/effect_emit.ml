(* 特定の型に依存しないEmit *)
open Effect
open Effect.Deep

module type EMIT = sig
  type t
  val emit : t -> unit
  val run : (unit -> 'a) -> (t -> unit) -> 'a
end

module Emit (S : sig type t end) : EMIT with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Emit : t -> unit Effect.t

  let emit (value: t) : unit = perform (Emit value)

  let run (f: unit -> 'a) (handler: t -> unit) : 'a =
    match_with f ()
    { retc = (fun result -> result);
      exnc = (fun e -> raise e);
      effc = (fun (type b) (eff: b Effect.t) ->
        match eff with
        | Emit value -> Some (fun (k: (b, _) continuation) ->
            handler value;
            continue k ())
        | _ -> None)
    }

end

module StringEmit = Emit (struct type t = string end)

let exampleStringEmit () =
  StringEmit.emit "Hello World"

let runExampleStringEmit () = StringEmit.run exampleStringEmit (fun value -> Printf.printf "Emitted: %s\n" value)