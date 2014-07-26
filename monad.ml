module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
end

module type SEQUENCE = sig
  type t
  val start : t
  val next : t -> t
end

module rec SequenceMonad : functor(Seq : SEQUENCE) -> sig
  include MONAD
  val run : 'a m -> 'a
  val next : Seq.t m 
  val get : Seq.t m
  val ( >> ) : 'a m -> 'b m -> 'b m
  val liftM : ('a -> 'b) -> 'a m -> 'b m
  val start : unit -> Seq.t m 
  (* val forever : 'a m -> 'b m *)
end = functor(Seq : SEQUENCE) -> struct
  type 'a m = Seq.t -> ('a * Seq.t)
  let (>>=) m f = fun s -> 
    let x, s' = m s in f x s'

  let return a : 'a m  = fun s -> a, s 

  let start () = fun s -> Seq.start, s

  let run m = 
    let x, s = m Seq.start in x 

  let next = fun s -> s, (Seq.next s)
 
  let get = fun s -> s, s

  let (>>) m k = m >>= (fun _ -> k)
  let liftM f s = s >>= fun x -> return (f x)
  (* let forever a =  *)
    (* let rec a' = lazy (a >> (Lazy.force a')) in (Lazy.force a') *)

end
module TypeParameter = struct
  type t = int
end

module StringMap = Map.Make(String)

(* module Env : sig *)
(*   include STATE *)
(*   val to_string : t -> string *)
(* end = struct *)
(*   type t = { *)
(*     next_variable_id : int; *)
(*     next_variable_name : char; *)
(*     bindings : TypeParameter.t Map.Make(String).t; *)
(*   } *)
  
(*   let empty = { *)
(*     next_variable_id = 0; *)
(*     next_variable_name = 'a'; *)
(*     bindings = StringMap.empty *)
(*   } *)

(*   let to_string = function *)
(*     | { next_variable_id = _; *)
(*         next_variable_name = nvn; *)
(*         _ } -> *)
(*       Char.escaped nvn *)

(* end *)



(* module EnvStateMonad = StateMonad(Env) *)

(* open EnvStateMonad *)


module EnvSequenceMonad : sig
  include MONAD
  type t = int * char
  val run : 'a m -> 'a
  val next_id : t m 
  val next_name : t m
  (* val get_id : t m *)
  (* val get_name : t m *)
  val start : unit -> t m 
  (* val forever : 'a m -> 'b m *)
  val liftM : ('a -> 'b) -> 'a m -> 'b m
  val (>>) : 'a m -> 'b m -> 'b m
end = struct
  type t = int * char
  let start_val = 0, 'a'

  type 'a m = t -> ('a * t)

  let (>>=) m f = fun s -> 
    let x, s' = m s in f x s'

  let return a : 'a m  = fun s -> a, s 

  let start () = fun s -> start_val, s

  let run m = 
    let x, s = m start_val in x 

  let next_id = fun s -> 
    let id, n = s in 
    s, (succ id, n)
  
  let next_name = fun s -> 
    let id, n = s in
    let nn = n |> Char.code |> succ |> Char.chr in
    s, (id, nn)
    
  let get = fun s -> s, s
  let (>>) m k = m >>= (fun _ -> k)
  let liftM f s = s >>= fun x -> return (f x)
end

open EnvSequenceMonad

let _ =
  let printM = liftM (fun x -> let id,n = x in Printf.printf "(%d, %c)\n" id n ; x) in
  let blah2 =
    let current = ref (start ()) in
    for i = 1 to 46 do
      if i mod 3 = 0 then
        current := !current >> printM next_name;
      current := (!current >> (printM next_id))
    done;
    !current
  in
  run blah2
