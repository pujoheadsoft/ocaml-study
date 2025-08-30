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
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff: b Effect.t) ->
        match eff with
        | Emit value -> Some (fun (k: (b, _) continuation) ->
            handler value;
            continue k ())
        | _ -> None)
    }

end

module StringEmit = Emit (struct type t = string end)

let emit_hello_world () =
  StringEmit.emit "Hello World"

let example_emit () =
  StringEmit.run emit_hello_world (fun value -> Printf.printf "Emitted: %s\n" value)