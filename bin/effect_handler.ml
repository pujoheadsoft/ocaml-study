open Effect
open Effect.Deep
open Effect.Shallow

type _ Effect.t += Ask: 'a Effect.t

let ask () = perform (Ask)

module type READER = sig  
  type t  
  val ask : unit -> t  
  val run : (unit -> 'a) -> init:t -> 'a  
end  
  
module Reader (S : sig type t end) : READER with type t = S.t = struct  
  type t = S.t  
    
  type _ Effect.t += Ask : t Effect.t  
    
  let ask () = perform Ask  
    
  let run f ~init =  
    let rec loop : type a r. t -> (a, r) continuation -> a -> r =  
      fun env k x ->  
        continue_with k x  
        { retc = (fun result -> result);  
          exnc = (fun e -> raise e);  
          effc = (fun (type b) (eff: b Effect.t) ->  
            match eff with  
            | Ask -> Some (fun (k: (b,r) continuation) ->  
                    loop env k env)  
            | _ -> None)  
        }  
    in  
    loop init (fiber f) ()  
end

module IntReader = Reader (struct type t = int end)  
module StringReader = Reader (struct type t = string end) 

let example () =  
  let value = IntReader.ask () in  
  Printf.printf "Got value: %d\n" value;  
  let another = IntReader.ask () in  
  Printf.printf "Got same value: %d\n" another  
  
let _ = IntReader.run example ~init:42