open Effect
open Effect.Deep

type _ Effect.t += Ask: 'a Effect.t

let ask () = perform (Ask)

