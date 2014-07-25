module Expr = struct
  type t =
    | Ident    of string
    | Apply    of t * t
    | Call     of t * t list
    | Function of string list * t
    | Fragment of string * t
    | Let      of string * t * t
    | Letrec   of string * t * t

  let rec to_string = function 
    | Ident s -> s
    | Apply (fn, arg) -> 
      Printf.sprintf "%s[ %s ]" (to_string fn) (to_string arg)

    | Let (v, defn, body) ->
      Printf.sprintf "(let %s = %s in %s)" v (to_string defn) (to_string body)

    | Letrec (v, defn, body) ->
      Printf.sprintf "(let rec %s = %s in %s)" v (to_string defn) (to_string body)

    | Function (vl, body) ->
      Printf.sprintf "fn(%s) { %s }" 
        (List.fold_left (fun a b -> (if a <> "" then a ^ "," else a) ^ b ) "" vl)
        (to_string body)
 
    | Fragment (v, body) -> (to_string body) 
    | Call (fn, args) ->
      Printf.sprintf "%s( %s )" (to_string fn) 
        (match args with
         | [] -> ""
         | hd::tl -> (List.fold_left (fun a e -> a ^ ", " ^ (to_string e)) (to_string hd) tl))

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

module rec TypeVariable : sig
  type t = {
    id : int;
    mutable name: string;
    mutable instance: TypeParameter.t option;
  }    
  val create: unit -> t
  val name: t -> string
  val to_string: ?depth:int -> t -> string
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end = struct
  type t =
    {
      id : int;
      mutable name: string;
      mutable instance: TypeParameter.t option;
    }

  let create () = 
    global_env.next_variable_id <- global_env.next_variable_id + 1;
    { 
      id = global_env.next_variable_id - 1;
      name = "";
      instance = None;
    }
    
  let name tv = 
    if tv.name = "" then begin
      tv.name <- Char.escaped global_env.next_variable_name;
      global_env.next_variable_name <- Char.chr ((Char.code global_env.next_variable_name) + 1)
    end;
    tv.name
  
  let to_string ?depth:(lvl=0) tv = 
    match tv.instance with
    | None -> name tv
    | Some i -> TypeParameter.to_string ?depth:(Some lvl) i

  let compare t1 t2 = t2.id - t1.id
  let hash tv = tv.id
  let equal tv1 tv2 = tv1.id = tv2.id
end

and TypeOperator : sig 
  type t = { name : string; types : TypeParameter.t list }
  val create: string -> TypeParameter.t list -> t
  val to_string: ?depth:int -> t -> string
end = struct
  type t =
    {
      name : string;
      types : TypeParameter.t list;
    }
  let create n tl =
    {
      name = n;
      types = tl;
    }

  let rec to_string ?depth:(lvl=0) t =
    let string_repr = 
      match t.name, t.types with
      | _, [] -> t.name
                   
      | a, _::_::_::_ when a = Function.name -> 
        t.types |> List.rev
        |> (function
            (* hd is the last element, the return type (reversed, remember?) *)
            | hd::[] -> "() " ^ Function.name ^ " " ^ (TypeParameter.to_string hd)
                                                      
            | hd::tl ->
              let args_string = tl |> List.rev |> List.fold_left (fun a b -> 
                  if a <> "" then 
                    Printf.sprintf "%s %s %s" a Fun.name (TypeParameter.to_string ?depth:(Some (succ lvl)) b) 
                  else 
                    TypeParameter.to_string ?depth:(Some (succ lvl)) b) ""
              in
              Printf.sprintf "%s %s %s" args_string Function.name (TypeParameter.to_string ?depth:(Some (succ lvl)) hd)
            | [] -> assert false)
          
  
      | _, hd::tl::[] -> 
        let hd_name = (TypeParameter.to_string ?depth:(Some (succ lvl)) hd) in
        let tl_name = (TypeParameter.to_string ?depth:(Some (succ lvl)) tl) in
        Printf.sprintf "%s %s %s" hd_name t.name tl_name
          
      | _, _ ->
        t.types
        |> List.map TypeParameter.to_string 
        |> List.fold_left (fun a b -> a ^ " " ^ b) ""
        |> Printf.sprintf "%s %s" t.name
    in
    if (lvl <> 0) && 
       ((String.length string_repr) > 1) && 
       (not (string_repr.[0] = '(')) && 
       (not (List.mem string_repr ["int";"bool";"unit"])) then
      "(" ^ string_repr ^ ")"
    else
      string_repr
end
and TypeParameter : sig
  type t = Tp_tvar of TypeVariable.t | Tp_top  of TypeOperator.t
  val to_string: ?depth:int -> t -> string
end = struct
  type t =
    | Tp_tvar of TypeVariable.t
    | Tp_top  of TypeOperator.t
                   
  let to_string ?depth:(lvl=0) = function 
    | Tp_tvar tv -> TypeVariable.to_string ?depth:(Some lvl) tv
    | Tp_top top -> TypeOperator.to_string ?depth:(Some lvl) top
end

and Fun : sig 
  val create: TypeParameter.t -> TypeParameter.t -> TypeParameter.t
  val name: string
end = struct
  let name = "->"
  let create from_type to_type =
    TypeParameter.Tp_top
    {
      TypeOperator.name  = name;
      TypeOperator.types = [from_type; to_type]
    }

end
and Function : sig
  val create: TypeParameter.t list -> TypeParameter.t -> TypeParameter.t
  val name: string
end = struct
  let name = "+>"
  let create from_types to_type = 
    match from_types with 
    | x::xs ->
      TypeParameter.Tp_top 
      {
        TypeOperator.name  = name;
        TypeOperator.types = (* (List.rev *) from_types(* ) *) @ [to_type]
      }
    | _ -> assert false
end


let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)


(* Basic types are constructed with a nullary type constructor *)
let my_int  = TypeParameter.Tp_top (TypeOperator.create "int" [])
let my_bool = TypeParameter.Tp_top (TypeOperator.create "bool" [])
let my_unit = TypeParameter.Tp_top (TypeOperator.create "unit" [])

module TVSet = Set.Make(TypeVariable)
module StringMap = Map.Make(String)

exception ParseError of string
exception TypeError of string
exception UnificationError of string

let rec analyse_lowest_fragment node env non_generic =
  match node with
  | Expr.Fragment (_,b) -> analyse_lowest_fragment b env non_generic
  | e -> analyse e env non_generic 

and analyse_highest_fragment node env non_generic =
  match node with
  | Expr.Function(_,b) -> analyse_highest_fragment b env non_generic
  | e -> analyse e env non_generic 

and analyse node env non_generic = 
  match node with
  | Expr.Ident name -> get_type name env non_generic

  | Expr.Apply (fn, arg) -> 
    (* let fun_type = analyse fn env non_generic in *)
    let fun_type = analyse_highest_fragment fn env non_generic in
    let arg_type = analyse_highest_fragment arg env non_generic in
    let result_type_param = TypeParameter.Tp_tvar (TypeVariable.create ()) in
    unify (Fun.create arg_type result_type_param) fun_type;
    result_type_param
    
  | Expr.Fragment(v, body) -> 
    let arg_type = TypeVariable.create () in
    let arg_type_param = TypeParameter.Tp_tvar arg_type in
    let new_env  = StringMap.add v arg_type_param env in
    let new_non_generic = TVSet.add arg_type non_generic in
    let result_type = analyse body new_env new_non_generic in
    Fun.create arg_type_param result_type
                                     
  | Expr.Let (v, defn, body) ->
    let defn_type = analyse defn env non_generic in
    let new_env   = StringMap.add v defn_type env in
    analyse body new_env non_generic
  
  | Expr.Letrec (v, defn, body) ->
    let new_type = TypeVariable.create () in
    let new_type_param = TypeParameter.Tp_tvar new_type in
    let new_env  = StringMap.add v new_type_param env in
    let new_non_generic = TVSet.add new_type non_generic in
    let defn_type = analyse defn new_env new_non_generic in
    unify new_type_param defn_type;
    analyse body new_env non_generic

  | Expr.Function (vl, body) ->
    let arg_types = List.map (fun _ -> TypeVariable.create ()) vl in
    let arg_type_params = List.map (fun t -> TypeParameter.Tp_tvar t) arg_types in
    let new_env = List.fold_right2 StringMap.add vl arg_type_params env in
    let new_non_generic = List.fold_right TVSet.add arg_types non_generic in
    (* let result_type = analyse body new_env new_non_generic in *)
    let result_type = analyse_lowest_fragment body new_env new_non_generic in
    (* (List.iter (fun x -> x |> TypeParameter.to_string |> print_endline ) arg_type_params); *)
    Function.create arg_type_params result_type

  (* A function passed no arguments passes the empty tuple to
   * its' first fragment (unit type).
   *)
  | Expr.Call (Expr.Function(vl,f), []) ->
    let fn_type = analyse f env non_generic in
    let arg_type = my_unit in
    let result_type_param = TypeParameter.Tp_tvar (TypeVariable.create ()) in
    unify (Fun.create arg_type result_type_param) fn_type;
    result_type_param
    
  | Expr.Call (Expr.Function(vl,_) as fn, args) ->
    (* if (List.length vl) <> (List.length args)  *)
    (* then raise @@ UnificationError "Error: number of arguments does not match"; *)
    let fun_type = analyse fn env non_generic in
    let arg_types = List.map (fun a -> analyse_highest_fragment a env non_generic) args in
    let result_type_param = TypeParameter.Tp_tvar (TypeVariable.create ()) in
    let unifier = (Function.create arg_types result_type_param) in
    (* print_endline (TypeParameter.to_string fun_type); *)
    (* print_endline (TypeParameter.to_string unifier); *)
    unify unifier fun_type;
    result_type_param

  | Expr.Call(Expr.Call(_,_) as fn, args)
  | Expr.Call(Expr.Ident(_) as fn, args) ->
    let fun_type = analyse_highest_fragment fn env non_generic in
    let arg_types = List.map (fun a -> analyse_highest_fragment a env non_generic) args in
    let result_type_param = TypeParameter.Tp_tvar (TypeVariable.create ()) in
    unify (Function.create arg_types result_type_param) fun_type;
    result_type_param

  | Expr.Call(_,_) -> 
    raise @@ UnificationError "Cannot call anything other than a function"
    

and get_type name env non_generic =
   if StringMap.mem name env then
     fresh (StringMap.find name env) non_generic 
   else if is_integer_literal name then
     my_int
   else
     raise @@ ParseError ("Undefined symbol " ^ name)

and fresh t non_generic : TypeParameter.t =
  let mappings = Hashtbl.create 30 in
  let rec freshrec tp : TypeParameter.t =
    let p = prune tp in
    match p with 
    | TypeParameter.Tp_tvar tv -> 
      let non_generic_list = 
        List.map (fun a -> TypeParameter.Tp_tvar a) (TVSet.elements non_generic)
      in
      if is_generic tv non_generic_list then begin
        if not (Hashtbl.mem mappings p)  
        then Hashtbl.replace mappings p (TypeParameter.Tp_tvar (TypeVariable.create ()));
        Hashtbl.find mappings p
      end
      else p
    | TypeParameter.Tp_top(top) ->
      TypeParameter.Tp_top (TypeOperator.create top.TypeOperator.name 
                              (List.map (fun x -> freshrec x) top.TypeOperator.types))
  in
  freshrec t


(**
 * Unify the two types t1 and t2
 * 
 * Makes the types t1 and t2 the same
 *)
and unify t1 t2 : unit =
  let a = prune t1 in
  let b = prune t2 in
  match (a, b) with
  | (TypeParameter.Tp_tvar(tv), _) ->
    if a <> b then begin
      if occurs_in_type tv b then
        raise (TypeError "recursive unification");
      tv.TypeVariable.instance <- Some b
    end
  | (TypeParameter.Tp_top(top), TypeParameter.Tp_tvar(tv)) ->
    unify b a
  | (TypeParameter.Tp_top(top1), TypeParameter.Tp_top(top2)) ->
    (* Allow the type system to unify lambdas and functions *)
    (match (top1.TypeOperator.name, top2.TypeOperator.name) with 
     | a, b when (a = Fun.name && b = Function.name) || 
                 (a = Function.name && b = Fun.name) -> ()
     | a,b when a = b -> ()
     | _,_ -> 
       raise @@ TypeError ("Type mismatch " ^ (TypeOperator.to_string top1) ^ " != " ^ (TypeOperator.to_string top2));
    );
    let top1_types_size = (List.length top1.TypeOperator.types) in
    let top2_types_size = (List.length top2.TypeOperator.types) in
    if (top1_types_size <> top2_types_size ) 
    then raise (TypeError ("Type mismatch " ^ (TypeOperator.to_string top1) ^ " != " ^ (TypeOperator.to_string top2)));
    List.iter2 unify (top1.TypeOperator.types) (top2.TypeOperator.types)
  (* | _ -> raise (UnificationError "Not unified") *)
      
and prune (t:TypeParameter.t) = 
  match t with
  | TypeParameter.Tp_tvar(tv) ->
    (match tv.TypeVariable.instance with 
     | Some stv -> 
       tv.TypeVariable.instance <- Some (prune stv);
       stv
     | None -> t)
  | _ -> t

and is_generic (v:TypeVariable.t) non_generic = not (occurs_in v non_generic)

and occurs_in_type (v:TypeVariable.t) t2 =
  let pruned_t2 = prune t2 in
  match pruned_t2 with 
  | TypeParameter.Tp_tvar tv when tv = v -> true
  | TypeParameter.Tp_top top -> occurs_in v top.TypeOperator.types
  | _ -> false

and occurs_in (t:TypeVariable.t) types = List.exists (fun t2 -> occurs_in_type t t2) types

and is_integer_literal name = 
  try
    ignore (int_of_string name);
    true
  with Failure _ -> false


let try_exp env node =
  Printf.printf "%s :  " (Expr.to_string node);
  try
    print_endline @@ TypeParameter.to_string @@ analyse node env TVSet.empty
  with
    ParseError e | TypeError e ->
    print_endline e


let () =
  let var1      = TypeParameter.Tp_tvar ( TypeVariable.create ()) in
  let var2      = TypeParameter.Tp_tvar ( TypeVariable.create ()) in
  let pair_type = TypeParameter.Tp_top  ( TypeOperator.create "*" [var1; var2]) in
  let var3      = TypeParameter.Tp_tvar (TypeVariable.create ()) in
  let my_env = 
    StringMap.empty
    |> StringMap.add "pair" (Fun.create var1 (Fun.create var2 pair_type))
    |> StringMap.add "true" my_bool
    |> StringMap.add "cond" (Fun.create my_bool 
                               (Fun.create var3 
                                  (Fun.create var3 var3)))
    |> StringMap.add "zero" (Fun.create my_int my_bool)
    |> StringMap.add "pred" (Fun.create my_int my_int)
    |> StringMap.add "times" (Fun.create my_int (Fun.create my_int my_int))
  in
  let pair =
    (Expr.Call
       ((Expr.Call (
            Expr.Ident "pair",
          [(Expr.Call 
             ((Expr.Ident "f"), [(Expr.Ident "4")]))])),
        [(Expr.Call
           (Expr.Ident "f",
            [Expr.Ident "true"]))]))
  in
  
  let examples =
    [
      (* factorial *)
      (Expr.Letrec              (* letrec factorial = *)
         ("factorial",
          Expr.Function(["n"],  (* fun n -> *)
            Expr.Fragment("n",
             Expr.Apply (
               Expr.Apply (     (* cond (zero n) 1 *)
                 Expr.Apply     (* cond (zero n) *)
                   (Expr.Ident "cond",
                    Expr.Apply (Expr.Ident "zero", Expr.Ident "n")),
                 Expr.Ident "1"),
               Expr.Apply (     (* times n *)
                 Expr.Apply (Expr.Ident "times", Expr.Ident "n"),
                 Expr.Apply (
                   Expr.Ident "factorial",
                   Expr.Apply (Expr.Ident "pred", Expr.Ident "n")
                 ))))),          (* in *)
          Expr.Apply (Expr.Ident "factorial", Expr.Ident "5")));
      
      (* Should fail
       * fun x -> (pair (x 3) (x true))
       *)
      Expr.Function(["x"],
        Expr.Fragment(
          "x",
          Expr.Apply(
            Expr.Call(Expr.Ident "pair",
                      [Expr.Call(Expr.Ident "x", [Expr.Ident "3"])]),
            Expr.Call(Expr.Ident "x", [Expr.Ident "true"]))));

      (* (pair (f 3)) (f true) *)
      Expr.Apply(
        Expr.Apply(Expr.Ident "pair", Expr.Apply(Expr.Ident "f", Expr.Ident "4")),
        Expr.Apply(Expr.Ident "f", Expr.Ident "true"));
      
      (* let f = (fn x -> x) in ((pair (f 4)) (f true)) *)
      Expr.Let("f",
        Expr.Function(["x"],
          Expr.Fragment("x",
            Expr.Ident "x")), pair);
      
      (* fun f -> f f *)
      (* This should fail (recursive type definition) *)
      Expr.Function(["f"],
        Expr.Fragment("f",
          Expr.Call(Expr.Ident "f", [Expr.Ident "f"])));
      
      (* should fail *)
      (* let g = fun f -> 5 in g (g,true) *)
      Expr.Let("g", Expr.Function(["f"], Expr.Fragment("f", Expr.Ident "5")),
        Expr.Call(Expr.Ident "g", [Expr.Ident "g"; Expr.Ident "true"]));

      (* let g = fun f -> 5 in g (g) *)
      Expr.Let("g", Expr.Function(["f"], Expr.Fragment("f", Expr.Ident "5")),
        Expr.Call(Expr.Ident "g", [Expr.Ident "g"]));

      (* example that demonstrates generic and non-generic variables *)
      (* fun g -> let f = fun x -> g in pair (f 3, f true) *)
      Expr.Function(["g"],
        Expr.Fragment("g",
          Expr.Let("f",
            Expr.Function(["x"], 
              Expr.Fragment("x",
                Expr.Ident "g")),
            Expr.Apply(
              Expr.Apply(
                Expr.Ident "pair",
                Expr.Apply(Expr.Ident "f", Expr.Ident "3")),
              Expr.Apply(Expr.Ident "f", Expr.Ident "true")))));
      
      (* function composition *)
      (* fun f -> fun g -> fun arg -> f g arg *)
      Expr.Function(["f";"g";"arg"],
        Expr.Fragment("g",
          Expr.Fragment("arg",
            Expr.Apply(
              Expr.Ident "g",
              Expr.Apply(
                Expr.Ident "f",
                Expr.Ident "arg")))));

      Expr.Function(["f"],
        Expr.Fragment("f",
          Expr.Function(["g"],
            Expr.Fragment("g", Expr.Ident "g"))));
      
      Expr.Apply(
        Expr.Function(["f"],
          Expr.Fragment("f",
            Expr.Function(["g"],
              Expr.Fragment("g", Expr.Ident "g")))),
        Expr.Ident "true");

      Expr.Call(
        Expr.Function(["f"],
          Expr.Fragment("f",
            Expr.Function(["g"],
              Expr.Fragment("g", Expr.Ident "g")))),
        [Expr.Ident "true"]);

      Expr.Function(
        ["f"; "g"],
        Expr.Fragment("f", Expr.Fragment("g",
            Expr.Apply(Expr.Ident "g", Expr.Ident "f"))));

      (* Should fail *)
        Expr.Function(["f"],
          Expr.Fragment("f",Expr.Fragment("g",
            Expr.Apply(Expr.Ident "g", Expr.Ident "f"))));

        Expr.Function(["f";"g"],
          Expr.Fragment("f",Expr.Fragment("g",
            Expr.Apply(Expr.Ident "g", Expr.Ident "f"))));

        Expr.Function(["f";"g"],
          Expr.Fragment("f",Expr.Fragment("g",
            Expr.Call(Expr.Ident "g", [Expr.Ident "f"]))));

        Expr.Function(["f";"g";"h"],
          Expr.Fragment("f", Expr.Fragment("g", Expr.Fragment("h",
            Expr.Call(Expr.Ident "h", [
                        Expr.Call(Expr.Ident "g", [Expr.Ident "f"])])))));

      Expr.Call(
        Expr.Function(
          ["f"; "z"],
          Expr.Fragment("f",
            Expr.Fragment("z",
              Expr.Function(["g"],
                Expr.Fragment("g",
                  Expr.Ident "g"))))),
        [Expr.Ident "true"; Expr.Ident "true"]);
    ]
  in
  List.iter (fun ex -> try_exp my_env ex) examples



