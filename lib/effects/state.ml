open Effect
open Effect.Shallow

module type STATE = sig
  type t
  val get : unit -> t
  val put : t -> unit
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Get : t Effect.t | Put : t -> unit Effect.t

  let get () = perform Get

  let put value = perform (Put value)

  let run f ~init =
    let rec loop : type a r. t -> (a, r) continuation -> a -> r =
      fun state k x ->
        continue_with k x
        {
          retc = Fun.id;
          exnc = raise;
          effc = (fun (type b) (eff: b Effect.t) -> 
            match eff with
            | Get -> Some (fun (k: (b, r) continuation) ->
                loop state k state)
            | Put value -> Some (fun (k: (b, r) continuation) ->
                loop value k ())
            | _ -> None
          )
        }
      in
      loop init (fiber f) ()
end

module StringState = State(struct type t = string end)

let get_example () =
  let value = StringState.get() in
  Printf.printf "Got value: %s\n" value

let put_example () =
  let value = StringState.get() in
  Printf.printf "Got value: %s\n" value;
  
  StringState.put ("<<<" ^ value ^ ">>>");

  let new_value = StringState.get() in
  Printf.printf "Got new value: %s\n" new_value

let exec_get () =
  StringState.run get_example ~init:"Hello, world!"


let exec_put () =
  StringState.run put_example ~init:"Hello, world!"