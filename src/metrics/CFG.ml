open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator
open Graph
open MetricUtils

type info =
  { id : int
  ; data : string
  }

exception CfgBuildFailed
exception Found

module IdSet = Caml.Set.Make (struct
  type t = int

  let compare = compare
end)

module Node = struct
  type t = info
end

module Edge = struct
  type t = int

  let compare = compare
  let default = 0
end

module G = Imperative.Digraph.AbstractLabeled (Node) (Edge)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [ `Shape `Box ]
  let vertex_name v = string_of_int (G.V.label v).id
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let contains_branching exp =
  let find_branches fallback =
    { fallback with
      expr =
        (fun self expr ->
          match expr.pexp_desc with
          | Pexp_ifthenelse _ | Pexp_match _ | Pexp_function _ -> raise Found
          | _ -> fallback.expr self expr)
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
    | [] -> [ current ]
    | _ -> branches |> List.map ~f:sub |> List.concat
  in
  sub start_node
;;

let show_graph =
  G.iter_edges (fun v1 v2 ->
    printfn
      "[(%d: %s)---(%d: %s)]"
      (G.V.label v1).id
      (G.V.label v1).data
      (G.V.label v2).id
      (G.V.label v2).data)
;;

let distinct : G.vertex list -> G.vertex list =
  List.fold
    ~f:(fun acc x ->
      if List.exists ~f:(fun el -> (G.V.label el).id = (G.V.label x).id) acc
      then acc
      else x :: acc)
    ~init:[]
;;

(** Saves Control Flow Graph *)
let save ~new_file_name ~path_to_save g =
  let open Caml in
  let full_path = Base.String.concat [ path_to_save; new_file_name ] in
  let () =
    try
      let file = open_out full_path in
      let () = Dot.output_graph file g in
      let png_file_name =
        Base.String.concat [ Filename.chop_suffix new_file_name ".dot"; ".png" ]
      in
      let png_full_path = Base.String.concat [ path_to_save; png_file_name ] in
      [ "dot -Tpng "; full_path; " > "; png_full_path ]
      |> Base.String.concat
      |> Sys.command
      |> ignore
    with
    | Sys_error _ -> printf "Error"
  in
  ()
;;

let remove_seq_from_graph graph start_vertex =
  let open G in
  let rec find_non_seq current =
    match current with
    | _ when String.equal (G.V.label current).data "seq" ->
      G.succ graph current
      (*|> List.filter ~f:(fun v -> (G.V.label v).id > (G.V.label current).id)*)
      |> List.map ~f:find_non_seq
      |> List.concat
    | _ -> [ current ]
  in
  let rec reduce v =
    let next_non_seq = G.succ graph v |> List.map ~f:find_non_seq |> List.concat in
    List.iter ~f:(fun new_v -> G.add_edge graph v new_v) next_non_seq;
    next_non_seq
    |> List.filter ~f:(fun current_v -> G.V.compare v current_v <> 0)
    |> List.iter ~f:reduce
  in
  reduce start_vertex;
  G.iter_vertex
    (fun v ->
      if String.equal (G.V.label v).data "seq" then G.remove_vertex graph v else ())
    graph;
  graph
;;

let get_apply_op_name = function
  | Pexp_ident { txt = Lident s; _ } -> s
  | _ -> ""
;;

let get_nested_functions expr =
  let nested_functions = ref [] in
  let fallback = Ast_iterator.default_iterator in
  let it =
    { fallback with
      expr =
        (fun self expr ->
          match expr.pexp_desc with
          | Pexp_let (_, vb, exp) ->
            let new_nested_functions =
              vb
              |> List.filter ~f:(fun x ->
                   match x.pvb_expr.pexp_desc with
                   | Pexp_fun _ -> true
                   | _ -> false)
            in
            nested_functions := !nested_functions @ new_nested_functions;
            fallback.expr self expr
          | _ -> fallback.expr self expr)
    }
  in
  it.expr it expr;
  !nested_functions |> List.map ~f:(fun x -> x.pvb_pat |> get_name, x.pvb_expr)
;;

let is_fun = function
  | Pexp_fun _ -> true
  | _ -> false
;;

(** Build Control Flow Graph from OCaml Parsetree expression*)
let rec build_cfg current_value_binding =
  let expr_func = current_value_binding.pvb_expr in
  let fun_name = current_value_binding.pvb_pat |> get_name in
  let id_set = ref IdSet.empty in
  let open G in
  let g = create () in
  let first_id = 0 in
  let start_vertex = G.V.create { id = first_id; data = "start" } in
  let back_edges = ref [] in
  id_set := IdSet.add first_id !id_set;
  G.add_vertex g start_vertex;
  (* This huge function iterates on AST nodes recursively and build CFG *)
  let rec process_builder current_exp prev_vertex nested_exprs call_list =
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
      List.iter ~f:(fun ev -> G.add_edge g ev new_end_vertex) endings
    in
    let create_new_node () =
      let new_id = generate_id id_set in
      match current_exp.pexp_desc with
      | Pexp_let (_, vb, exp) ->
        let new_vertex = update_graph ~new_id ~name:"let" in
        let nested_functions =
          vb
          |> List.filter ~f:(fun x -> is_fun x.pvb_expr.pexp_desc)
          |> List.map ~f:(fun x -> get_name x.pvb_pat, x.pvb_expr)
        in
        let end_point = new_vertex |> end_branching g in
        let () =
          match distinct end_point with
          | [ v ] -> process_builder exp v (nested_exprs @ nested_functions) call_list
          | _ ->
            printf "Let: ";
            show_graph g;
            raise CfgBuildFailed
        in
        ()
      | Pexp_fun (_, _, _, exp) ->
        let new_vertex = update_graph ~new_id ~name:"fun" in
        process_builder exp new_vertex nested_exprs call_list
      | Pexp_open (_, exp) ->
        let new_vertex = update_graph ~new_id ~name:"open" in
        process_builder exp new_vertex nested_exprs call_list
      | Pexp_apply (ex, _) ->
        let operator_name = get_apply_op_name ex.pexp_desc in
        (* printf "I'm here!";
        List.iter ~f:(fun (_, x) ->  Pprintast.expression Format.std_formatter x) nested_exprs; *)
        let impl =
          nested_exprs |> List.find ~f:(fun x -> x |> fst |> String.equal operator_name)
        in
        let () =
          match impl with
          | Some (name, e) when not @@ List.exists ~f:(String.equal name) call_list ->
            let new_vertex = update_graph ~new_id ~name:(sprintf "fun: %s" name) in
            process_builder e new_vertex nested_exprs (name :: call_list)
          | _ ->
            let new_vertex = update_graph ~new_id ~name:"expr" in
            if String.equal operator_name fun_name
            then back_edges := (new_vertex, start_vertex) :: !back_edges
        in
        ()
      | Pexp_ifthenelse (_, exp1, Some exp2) ->
        let new_vertex = update_graph ~new_id ~name:"if_then_else" in
        process_builder exp1 new_vertex nested_exprs call_list;
        process_builder exp2 new_vertex nested_exprs call_list;
        add_end_point ~new_vertex
      | Pexp_match (_, exprs) | Pexp_function exprs ->
        let new_vertex = update_graph ~new_id ~name:"match" in
        List.iter ~f:(fun x -> process_builder x.pc_rhs new_vertex nested_exprs call_list) exprs;
        add_end_point ~new_vertex
      | Pexp_ident _ -> update_graph ~new_id ~name:"ident" |> ignore
      | Pexp_constant _ -> update_graph ~new_id ~name:"constant" |> ignore
      | Pexp_construct _ -> update_graph ~new_id ~name:"construct" |> ignore
      | Pexp_constraint (ex, _) -> process_builder ex prev_vertex nested_exprs call_list
      | _ -> update_graph ~new_id ~name:"undefined node" |> ignore
    in
    match current_exp.pexp_desc with
    | Pexp_sequence (exp1, exp2) ->
      let new_vertex = update_graph ~new_id:(-1) ~name:"seq" in
      process_builder exp1 new_vertex nested_exprs call_list;
      let end_point = new_vertex |> end_branching g in
      let () =
        match distinct end_point with
        | [ id ] -> process_builder exp2 id nested_exprs call_list
        | _ -> raise CfgBuildFailed
      in
      ()
    | _ -> create_new_node ()
  in
  process_builder expr_func start_vertex [] [ fun_name ];
  let g_without_seq = remove_seq_from_graph g start_vertex in
  !back_edges |> List.iter ~f:(fun (v1, v2) -> G.add_edge g_without_seq v1 v2);
  (* show_graph g_without_seq; *)
  g_without_seq
;;
