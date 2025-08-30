open Effect
open Effect.Shallow

(* こういうデフォルトのハンドラー ( ('a,'b) handler ) を作っておくと毎度同じものを書かないで済む *)
let default_handler =
  { retc = Fun.id;
    exnc = raise;
    effc = fun (type c) (_ : c Effect.t) -> None }

module type STATE = sig
  type t
  val get : unit -> t
  val put : t -> unit
  (* Effect Tutorial では unit を返していたが、HaskellとかのevalStateみたいに値を返せるようにした *)
  val run : (unit -> 'a) -> init:t -> 'a
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
        { default_handler with
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

let get_example2 () =
  let value = StringState.get() in
  "<<<" ^ value ^ ">>>"

let put_example () =
  let value = StringState.get() in
  Printf.printf "Got value: %s\n" value;
  
  StringState.put ("<<<" ^ value ^ ">>>");

  let new_value = StringState.get() in
  Printf.printf "Got new value: %s\n" new_value
  
let exec_get () =
  StringState.run get_example ~init:"Hello, world!"

let exec_get2 () =
  let value = StringState.run get_example2 ~init:"Value" in
  Printf.printf "Final value: %s\n" value

let exec_put () =
  StringState.run put_example ~init:"Hello, world!"