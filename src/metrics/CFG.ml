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

module Dot = Graph.Graphviz.Dot(struct
  include G (* use the graph module from above *)
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = string_of_int (G.V.label v).id
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

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

let show_graph = 
  G.iter_edges (fun v1 v2 -> printfn "[(%d: %s)---(%d: %s)]" (G.V.label v1).id (G.V.label v1).data (G.V.label v2).id (G.V.label v2).data) 
;;

let distinct : G.vertex list -> G.vertex list = 
  List.fold ~f:(fun acc x -> if (List.exists ~f:(fun el -> (G.V.label el).id = (G.V.label x).id) acc) then acc else x::acc) ~init:[]

(* Saves Control Flow Graph *)
let save ~new_file_name ~path_to_save g =
  let open Caml in
  let full_path = Base.String.concat [path_to_save; new_file_name] in
  let () =
    try
      let file = open_out full_path in
      let () = Dot.output_graph file g in
      let png_file_name = Base.String.concat [Filename.chop_suffix new_file_name ".dot"; ".png"] in
      let png_full_path = Base.String.concat [path_to_save; png_file_name] in
      ["dot -Tpng "; full_path; " > "; png_full_path] |> Base.String.concat |> Sys.command |> ignore;
      ()
    with Sys_error _ -> printf "Error";
  in ()
;;

(* Build Control Flow Graph from OCaml Parsetree expression*)
let build_cfg (expr_func : Parsetree.expression) =
  let id_set = ref IdSet.empty in
  
  let open G in
  let g = create () in
  let first_id = 0 in
  let start_vertex = G.V.create { id = first_id; data = "start" } in
  id_set := IdSet.add first_id !id_set;
  G.add_vertex g start_vertex;
  
  let rec process_builder current_exp prev_vertex = 
    let update_graph ~new_id ~name =
      let new_vertex = G.V.create { id = new_id; data = name } in
      G.add_vertex g new_vertex;
      G.add_edge g prev_vertex new_vertex;
      new_vertex
    in
    let add_end_point ~new_vertex =
      let endings = end_branching g new_vertex in
      let new_end_id = generate_id id_set in
      let new_end_vertex = G.V.create { id = new_end_id; data = "end_point" } in
      List.iter ~f: (fun ev -> G.add_edge g ev new_end_vertex) endings;
    in
    let create_new_node () =
      let new_id = generate_id id_set in
      match current_exp.pexp_desc with
      | Pexp_let (_, vb, exp) ->
      let new_vertex = update_graph ~new_id ~name:"let" in
      List.iter ~f:(fun x -> if contains_branching x.pvb_expr then process_builder x.pvb_expr new_vertex) vb;
      let end_point = new_vertex |> end_branching g in
      let () = 
        match distinct end_point with
        | [v] -> process_builder exp v
        | _ -> raise SomethingIsWrong 
      in ()
      | Pexp_fun (_, _, _, exp) ->
      let new_vertex = update_graph ~new_id ~name:"fun" in
      process_builder exp new_vertex;
      | Pexp_open (_, exp) ->
      let new_vertex = update_graph ~new_id ~name:"open" in
      process_builder exp new_vertex;
      | Pexp_apply _ ->
      update_graph ~new_id ~name:"expr" |> ignore;
      | Pexp_ifthenelse (_, exp1, Some exp2) ->
      let new_vertex = update_graph ~new_id ~name:"if_then_else" in
      process_builder exp1 new_vertex;
      process_builder exp2 new_vertex;
      add_end_point ~new_vertex;
      | Pexp_match (_, exprs)
      | Pexp_function (exprs) ->
      let new_vertex = update_graph ~new_id ~name:"match" in
      List.iter ~f:(fun x -> process_builder x.pc_rhs new_vertex) exprs;
      add_end_point ~new_vertex;
      | Pexp_ident _      -> update_graph ~new_id ~name:"ident" |> ignore;
      | Pexp_constant _   -> update_graph ~new_id ~name:"constant" |> ignore
      | Pexp_construct _  -> update_graph ~new_id ~name:"construct" |> ignore
      | _                 -> update_graph ~new_id ~name:"undefined node" |> ignore
    in
    match current_exp.pexp_desc with
    | Pexp_sequence (exp1, exp2) ->
      process_builder exp1 prev_vertex;
      let end_point = prev_vertex |> end_branching g in
      let () =
        match distinct end_point with
        | [id] -> process_builder exp2 id;
        | _ -> raise SomethingIsWrong
      in ()
    | _ -> create_new_node ()
  in
  process_builder expr_func start_vertex;
  show_graph g;
  (*let open Caml in
  let new_file_name = "file.txt" in
  let dir = "~/source/new_linter_dir/mylinter/cfgs/file.txt" in
  let res = Sys.command "touch ~/source/new_linter_dir/mylinter/cfgs/file.txt" in printf "\nresp:%d\n" res;
  let () =
    try
      let file = open_out_bin "../../cfgs/cfg.dot" in
      let () = Dot.output_graph file g in
      ()
    with
    | Sys_error _ -> ()
  in*)
  g
;;