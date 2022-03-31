open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

type vertex = {
  id : int;
  info : string
};;

exception SomethingIsWrong
exception Found


let contains_branching exp =
  let find_branches fallback =
    {
      fallback with
      expr =
        (fun self expr ->
          match expr.pexp_desc with
          | Pexp_ifthenelse _  | Pexp_match (_, _) | Pexp_function _ -> raise Found
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
  let new_id = (List.length !id_set) in
  id_set := new_id::!id_set;
  new_id
;;

let print_graph graph vertexes =
  graph
  |> List.rev
  |> List.map (*Get info about each vertex*) 
     ~f:(fun (fstv, sndv) -> 
         let vertex1 = List.find ~f:(fun x -> x.id = fstv) vertexes in
         let vertex2 = List.find ~f:(fun x -> x.id = sndv) vertexes in
         match (vertex1, vertex2) with
         | (Some v1, Some v2) -> ({id = fstv; info = v1.info}, {id = sndv; info = v2.info})
         | _ -> raise SomethingIsWrong)
  |> List.iter ~f:(fun x -> printf "[(id:%i info: %s); (id:%i info: %s)]\n" ((fst x).id) ((fst x).info) ((snd x).id) ((snd x).info))
;;

let end_branching graph start_node =
  let rec sub current =
    let branches = !graph |> List.filter ~f: (fun x -> current = fst x) in
    match branches with
    | [] -> [current]
    | _ -> branches |> List.map ~f: (fun x -> sub (snd x)) |> List.concat
  in
  sub start_node
;;

let are_equal_edges e1 e2 = ((fst e1) = (fst e2)) && ((snd e1) = (snd e2))

let add_edge edge graph =
  if not @@ List.exists ~f:(fun el -> are_equal_edges edge el) !graph then
    graph := edge::!graph
;;

let add_vertex_info new_id new_info vertexes =
  vertexes := { id = new_id; info = new_info }::!vertexes
;;

let distinct_list li = li |> List.fold ~f:(fun acc x -> if not @@ List.exists ~f:(fun el ->el = x) acc then x::acc else acc) ~init:[]


(* Build Control Flow Graphh from OCaml Parsetree expression*)
let build_cfg (expr_func : Parsetree.expression) =
  let graph : (int * int) list ref = ref [] in
  let id_set : int list ref = ref [0] in
  let vertexes : vertex list ref = ref [{ id = 0; info = "start" }] in

  let rec process_builder current_exp prev_id = 
    match current_exp.pexp_desc with
    | Pexp_let (_, vb, exp) ->
    let new_id = generate_id id_set in
    add_vertex_info new_id "Let" vertexes;
    add_edge (prev_id, new_id) graph;
    List.iter ~f:(fun x -> if contains_branching x.pvb_expr then process_builder x.pvb_expr new_id) vb;
    process_builder exp new_id;
    | Pexp_fun (_, _, _, exp) ->
    let new_id = generate_id id_set in
    add_vertex_info new_id "Fun" vertexes;
    add_edge (prev_id, new_id) graph;
    process_builder exp new_id;
    | Pexp_apply _ ->
    let new_id = generate_id id_set in
    add_vertex_info new_id "Apply" vertexes;
    add_edge (prev_id, new_id) graph;
         (* List.iter ~f:(fun x -> process_builder (snd x) new_id) exprs; *)
     (* | Pexp_while (_, exp) ->
          let new_id = generate_id () in
          add_edge (prev_id, new_id);
          Caml.Format.printf "\nWhile: id: %d\n" new_id;
          process_builder exp new_id;
          let endings = new_id |> end_branching |> distinct_list in
          List.iter ~f: (fun x -> add_edge (x, new_id)) endings;*)
    | Pexp_ifthenelse (_, exp1, Some exp2) ->
    let new_id = generate_id id_set in
    add_vertex_info new_id "If-Then-Else" vertexes;
    add_edge (prev_id, new_id) graph;
    process_builder exp1 new_id;
    process_builder exp2 new_id;
    let endings = end_branching graph new_id in
    let new_end_id = generate_id id_set in
    add_vertex_info new_end_id "End Point" vertexes;
    List.iter ~f: (fun ev -> add_edge (ev, new_end_id) graph) endings;
    | Pexp_match (_, exprs) 
    | Pexp_function (exprs) ->  
    let new_id = generate_id id_set in
    add_edge (prev_id, new_id) graph;
    add_vertex_info new_id "Match" vertexes;
    List.iter ~f:(fun x -> process_builder x.pc_rhs new_id) exprs;
    let endings = end_branching graph new_id in
    let new_end_id = generate_id id_set in
    add_vertex_info new_end_id "End Point" vertexes;
    List.iter ~f: (fun ev -> add_edge (ev, new_end_id) graph) endings;
    | Pexp_sequence (exp1, exp2) ->
    process_builder exp1 prev_id;
    let end_point = prev_id |> end_branching graph |> distinct_list in
    let process_sequence () =
      match end_point  with
      | [id] -> process_builder exp2 id;
      | _ -> raise SomethingIsWrong
    in
    process_sequence ()
    | Pexp_ident (_) ->
    let new_id = generate_id id_set in
    add_edge (prev_id, new_id) graph;
    add_vertex_info new_id "Ident" vertexes;
    | Pexp_constant (_) -> 
    let new_id = generate_id id_set in
    add_edge (prev_id, new_id) graph;
    add_vertex_info new_id "Constant" vertexes;
    | _ -> ();
  in
  process_builder expr_func 0;
(*  print_graph !graph !vertexes;*)
  (!id_set, !graph)
;;
