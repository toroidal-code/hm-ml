module Expr = struct
  type t =
    | Ident  of string
    | Apply  of t * t
    | Lambda of string * t
    | Let    of string * t * t
    | Letrec of string * t * t
    | None

  let rec to_string = function 
    | Ident s -> s
    | Apply (fn, arg) -> 
      Printf.sprintf "(%s %s)" (to_string fn) (to_string arg)

    | Lambda (v, body) ->
      Printf.sprintf "(fun %s -> %s)" v (to_string body)

    | Let (v, defn, body) ->
      Printf.sprintf "(let %s = %s in %s)" v (to_string defn) (to_string body)

    | Letrec (v, defn, body) ->
      Printf.sprintf "(let rec %s = %s in %s)" v (to_string defn) (to_string body)
    | None -> "None"
end

type env = 
  {
    mutable next_variable_id : int;
    mutable next_variable_name : char
  }
;;

let global_env = 
  { 
    next_variable_id = 0;
    next_variable_name = 'a' 
  } 
;;

module TypeVariable = struct
  type t =
    {
      id : int;
      mutable name: string;
      mutable instance: Expr.t;
    }

  let create () = 
    global_env.next_variable_id <- global_env.next_variable_id + 1;
    { 
      id = global_env.next_variable_id - 1;
      name = "";
      instance = Expr.None;
    }
    
  let name tv = 
    if tv.name = "" then begin
      tv.name <- Char.escaped global_env.next_variable_name;
      global_env.next_variable_name <- Char.chr ((Char.code global_env.next_variable_name) + 1)
    end;
    tv.name
  
  let to_string tv = 
    match tv.instance with
    | Expr.None -> tv.name
    | _ -> Expr.to_string tv.instance

  let compare t1 t2 = t2.id - t1.id
end

module TypeOperator = struct
  type t =
    {
      name : string;
      types : TypeVariable.t list;
    }
  let create n tl =
    {
      name = n;
      types = tl;
    }

  let to_string t =
    match t.types with
    | [] -> t.name

    | hd::tl::[] -> 
      Printf.sprintf "(%s %s %s)" (TypeVariable.to_string hd) t.name (TypeVariable.to_string tl)

    | _ -> 
      t.types
      |> List.map TypeVariable.to_string 
      |> List.fold_left (fun a b -> a ^ " " ^ b) ""
      |> Printf.sprintf "%s %s" t.name
end

module Function = struct
  type t = TypeOperator.t
  let to_string = TypeOperator.to_string
  let create from_type to_type =
    {
      TypeOperator.name = "->";
      TypeOperator.types = [from_type; to_type]
    }
end

(* Basic types are constructed with a nullary type constructor *)
let my_int  = TypeOperator.create "int" []
let my_bool = TypeOperator.create "bool" []

module TVSet = Set.Make(TypeVariable)
module StringMap = Map.Make(String)

let rec analyse node env ?non_generic:(ng=TVSet.empty) = 
  match node with
  | Expr.Ident name -> get_type name env ?non_generic

  | Expr.Apply (fn, arg) -> 
    let fun_type = analyse fn env non_generic in
    let arg_type = analyse arg env non_generic in
    let result_type = TypeVariable.create () in
    unify (Function.create arg_type result_type) fun_type;
    result_type

  | Expr.Lambda (v, body) -> 
    let arg_type = TypeVariable.create () in
    let new_env  = StringMap.add v arg_type env in
    let new_non_generic = TVSet.add arg_type non_generic in
    let result_type = analyse body new_env new_non_generic in
    Function.create arg_type result_type
                                     
and get_type name env non_generic = 2
