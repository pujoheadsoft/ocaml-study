open Effect

module type STATE = sig
  type t
  val get : unit -> t
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Get : t Effect.t

  let get () = perform Get
end