open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator


type node =
  | IfThenElse
  | Matching
  | Statement
  | EndPoint

let graph : (int * int) list ref = ref []
let id_set : int list ref = ref [0]

let generate_id () = 
  let new_id = (List.length !id_set) in
  id_set := new_id::!id_set;
  new_id
;;

(*
let contains_branching exp =
  let is_branching_exp  = ref false in
  let it = {
    Ast_iterator.default_iterator with
    expr =
      (fun self expr ->
        match expr.pexp_desc with
        | Pexp_ifthenelse (_, _, _)  
        | Pexp_match (_, _) -> is_branching_exp := true;
        | _ -> ()
      )
  }
  in
  0
;;*)

let print_graph () =
  List.iter ~f: (fun x -> Caml.Format.printf "(%d, %d)" (fst x) (snd x)) !graph
;;

let end_branching start_node =
  let rec sub current =
    let branches = !graph |> List.filter ~f: (fun x -> current = fst x) in
    match branches with
    | [] -> [current]
    | _ -> branches |> List.map ~f: (fun x -> sub (snd x)) |> List.concat
  in
  sub start_node
;;

let are_equal_edges e1 e2 = ((fst e1) = (fst e2)) && ((snd e1) = (snd e2))

let add_edge edge =
  if not @@ List.exists ~f:(fun el -> are_equal_edges edge el) !graph then
    graph := edge::!graph
;;

exception SomethingIsWrong

let distinct_list li = li |> List.fold ~f:(fun acc x -> if not @@ List.exists ~f:(fun el ->el = x) acc then x::acc else acc) ~init:[]


(* Build Control Flow Graphh from OCaml Parsetree expression*)
let build_cfg (expr_func : Parsetree.expression) =
  let rec process_builder current_exp prev_id = 
    Caml.Format.printf "\n======================\n";
    Pprintast.expression Format.std_formatter current_exp;
    match current_exp.pexp_desc with
    | Pexp_let (_, vb, exp) ->
        let new_id = generate_id () in
        Caml.Format.printf "\nLet: Id: %d\n" new_id;
        printf "Next exp: size:%d" (List.length vb);
        List.iter ~f:(fun x -> Pprintast.expression Format.std_formatter x.pvb_expr) vb;
        (*Pprintast.expression Format.std_formatter exp;*)
        add_edge (prev_id, new_id);
        process_builder exp new_id;
    | Pexp_fun (_, _, _, exp) ->
        let new_id = generate_id () in
        Caml.Format.printf "\nFun: Id: %d\n" new_id;
        add_edge (prev_id, new_id);
        process_builder exp new_id;
    | Pexp_apply (_, exprs) ->
        let new_id = generate_id () in
        Caml.Format.printf "\nApply\n";
        add_edge (prev_id, new_id);
       (* List.iter ~f:(fun x -> process_builder (snd x) new_id) exprs; *)
    | Pexp_ifthenelse (_, exp1, Some exp2) ->
        let new_id = generate_id () in
        printf "\nIfThenElse Id: %d\n" new_id;
        add_edge (prev_id, new_id);
        process_builder exp1 new_id;
        process_builder exp2 new_id;
        let endings = end_branching new_id in
        let new_end_id = generate_id () in
        List.iter ~f: (fun ev -> add_edge (ev, new_end_id)) endings;
    | Pexp_match (_, exprs) ->
        let new_id = generate_id () in
        add_edge (prev_id, new_id);
        printf "\nMatching: Id: %d" new_id;
        List.iter ~f:(fun x -> process_builder x.pc_rhs new_id) exprs;
        let endings = end_branching new_id in
        let new_end_id = generate_id () in
        List.iter ~f: (fun ev -> add_edge (ev, new_end_id)) endings;
    | Pexp_sequence (exp1, exp2) ->
        process_builder exp1 prev_id;
        let end_point = prev_id |> end_branching |> distinct_list in
        let process_sequence () =
          match end_point  with
          | [id] -> process_builder exp2 id;
          | _ -> 
            List.iter ~f: (fun a -> printf "%d" a) end_point;
            raise SomethingIsWrong
        in
        process_sequence ()
    | Pexp_ident (_) -> 
        let new_id = generate_id () in
        add_edge (prev_id, new_id);
        printf "\nIdent: Id: %d\n" new_id;
    | Pexp_constant (_) -> 
        let new_id = generate_id () in
        add_edge (prev_id, new_id);
        printf "\nConstant: Id: %d\n" new_id;
    | _ -> printf "\nOther\n";
  in
  process_builder expr_func 0
;;
(*
let process fallback =
  { 
    fallback with
    structure = 
    (fun self st ->
    Caml.Format.printf "\n======================\n";
    Pprintast.structure Format.std_formatter st;
    fallback.structure self st
    )
  }
;;
*)
let run parsetree =
  let it = {
  Ast_iterator.default_iterator with
   expr = 
     (fun _ ex ->
       match ex.pexp_desc with
       | Pexp_fun (lab, _, pat, fun_ex) ->
         build_cfg ex;
         ()
       | _ -> Caml.Format.printf "======Other========\n";
     )
  }
  in
  it.structure it parsetree;
  print_graph ();
  let complexity = (List.length !graph) - (List.length !id_set) + 2 in
  printf "\nComplexity: %d\n" complexity;
;;