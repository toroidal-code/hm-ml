module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t

  val (>>): 'a t -> 'b t -> 'b t
  (* val fail: string -> 'a t *)
end

module type STATE = sig
  type t
  val empty : t
end

module type STATE_MONAD = 
  functor(State : STATE) -> sig 
    include MONAD
    val get: State.t t
    val put: State.t -> unit t
    val access: 'a t -> 'a
  end

module StateMonad : STATE_MONAD =
  functor(State : STATE) -> struct
    type state = State.t
    type 'a t = state -> ('a * state)
    let (>>=) m f = fun s -> let x, s' = m s in f x s'
    let (>>) m k = m >>= (fun _ -> k)
    let return a = fun s -> (a, s)
    let access m = let x, s = m State.empty in x
    let put s = fun _ -> (), s
    let get = fun s -> s, s
  end
