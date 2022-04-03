open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator
open Graph

type info = {
  id   : int;
  data : string
};;

exception SomethingIsWrong
exception Found

module IdSet =
  Caml.Set.Make (struct 
                   type t = int
                   let compare = compare
                 end);;

module Node = struct
  type t = info
end
module Edge = struct
  type t = int
  let compare = compare
  let default = 0
end
module G = Imperative.Digraph.AbstractLabeled(Node)(Edge)

let contains_branching exp =
  let find_branches fallback =
    {
      fallback with
      expr =
        (fun self expr ->
          match expr.pexp_desc with
          | Pexp_ifthenelse _  | Pexp_match _ | Pexp_function _ -> raise Found
          | _ -> fallback.expr self expr;
        )
    }
  in
  try 
    let iter = find_branches Ast_iterator.default_iterator in
    iter.expr iter exp;
    false
  with
  | Found -> true
;;

let generate_id id_set =
  match IdSet.max_elt_opt !id_set with
  | Some elt ->
  let new_id = elt + 1 in
  id_set := IdSet.add new_id !id_set;
  new_id
  | None -> 
  id_set := IdSet.add 0 !id_set;
  0
;;

let end_branching g start_node =
  let open G in
  let rec sub current =
    let branches = G.succ g current in
    match branches with
    | [] -> [current]
    | _ -> branches |> List.map ~f:sub |> List.concat
  in
  sub start_node
;;

(* Build Control Flow Graph from OCaml Parsetree expression*)
let build_cfg (expr_func : Parsetree.expression) =
  let id_set = ref IdSet.empty in
  
  let open G in
  let g = create () in
  let start_vertex = G.V.create { id = 0; data = "start" } in
  G.add_vertex g start_vertex;
  
  let rec process_builder current_exp prev_vertex = 
    match current_exp.pexp_desc with
    | Pexp_let (_, vb, exp) ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "let" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex ;
    List.iter ~f:(fun x -> if contains_branching x.pvb_expr then process_builder x.pvb_expr new_vertex) vb;
    process_builder exp new_vertex;
    | Pexp_fun (_, _, _, exp) ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "fun" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex ;
    process_builder exp new_vertex;
    | Pexp_apply _ ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "expr"}  in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex;
    | Pexp_ifthenelse (_, exp1, Some exp2) ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "if-then-else" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex;
    process_builder exp1 new_vertex;
    process_builder exp2 new_vertex;
    let endings = end_branching g new_vertex in
    let new_end_id = generate_id id_set in
    let new_end_vertex = G.V.create { id = new_end_id; data = "end-point" } in
    List.iter ~f: (fun ev -> G.add_edge g ev new_end_vertex) endings;
    | Pexp_match (_, exprs) 
    | Pexp_function (exprs) ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "match" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex;
    List.iter ~f:(fun x -> process_builder x.pc_rhs new_vertex) exprs;
    let endings = end_branching g new_vertex in
    let new_end_id = generate_id id_set in
    let new_end_vertex = G.V.create { id = new_end_id; data = "end-point" } in
    List.iter 
      ~f: (fun ev -> G.add_edge g ev new_end_vertex) 
      endings;
    | Pexp_sequence (exp1, exp2) ->
    process_builder exp1 prev_vertex;
    let end_point = prev_vertex |> end_branching g in
    let process_sequence () =
      match end_point  with
      | [id] -> process_builder exp2 id;
      | _ -> raise SomethingIsWrong
    in
    process_sequence ()
    | Pexp_ident (_) ->
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "ident" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex;
    | Pexp_constant (_) -> 
    let new_id = generate_id id_set in
    let new_vertex = G.V.create { id = new_id; data = "constant" } in
    G.add_vertex g new_vertex;
    G.add_edge g prev_vertex new_vertex;
    | _ -> ();
  in
  
  process_builder expr_func start_vertex;
  G.nb_edges g, G.nb_vertex g
;;
