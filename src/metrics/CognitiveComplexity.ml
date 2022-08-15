open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

(** Get complexity of logical expression *)
let get_logical_operators_list exp =
  let rec sub local_exp =
    match local_exp.pexp_desc with
    | Pexp_apply (ex, li) ->
      let result =
        match ex.pexp_desc with
        | Pexp_ident { txt = Lident s; _ } ->
          let exprs = List.map ~f:snd li in
          let new_operator_list =
            match exprs with
            | [ arg1; arg2 ] when String.equal "&&" s || String.equal "||" s ->
              let first_result = sub arg1 in
              let second_result = sub arg2 in
              List.concat [ first_result; [ s ]; second_result ]
            | _ -> []
          in
          new_operator_list
        | _ -> []
      in
      result
    | _ -> []
  in
  sub exp
;;

(** checks if the operator is logical operator: && or || *)
let is_logical_operator expr =
  let open Longident in
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident s; _ } -> String.equal "&&" s || String.equal "||" s
  | _ -> false
;;

(** Process Cognitive complexity *)
let run parsetree info =
  let open CCComplexity in

  let cl = ref [] in
  let fun_names = get_fun_names parsetree in
  printf "Fun names: ";
  List.iter ~f:(printf "%s ") fun_names;
  let function_index = ref 0 in
  let it =
    { Ast_iterator.default_iterator with
      expr =
        (fun _ ex ->
          match ex.pexp_desc with
          | Pexp_function _ | Pexp_fun (_, _, _, _) ->
            let nesting_level = ref 0 in
            let cognitive_complexity = ref 0 in
            let process_function fallback =
              { fallback with
                expr =
                  (fun self currentExpr ->
                    match currentExpr.pexp_desc with
                    | Pexp_while _
                    | Pexp_for _
                    | Pexp_ifthenelse _
                    | Pexp_function _
                    | Pexp_match _
                    | Pexp_try _ ->
                      cognitive_complexity := 1 + !nesting_level + !cognitive_complexity;
                      incr nesting_level;
                      fallback.expr self currentExpr;
                      decr nesting_level
                    | Pexp_apply (op, _) ->
                      if is_logical_operator op
                      then (
                        let res = get_logical_operators_list currentExpr in
                        List.iter ~f:(fun x -> printfn "%s; " x) res;
                        let complexity_delta =
                          res
                          |> List.fold
                               ~f:(fun acc current_operator ->
                                 let prev_op, complexity = acc in
                                 match String.equal prev_op current_operator with
                                 | true -> current_operator, complexity
                                 | _ -> current_operator, complexity + 1)
                               ~init:("", 0)
                          |> snd
                        in
                        cognitive_complexity := !cognitive_complexity + complexity_delta)
                      else fallback.expr self currentExpr
                    | Pexp_let (Asttypes.Recursive, _, _) ->
                      incr cognitive_complexity;
                      fallback.expr self currentExpr
                    | _ -> fallback.expr self currentExpr)
              }
            in
            let local_it = process_function Ast_iterator.default_iterator in
            local_it.expr local_it ex;
            let local_fun_name = List.nth fun_names !function_index in
            incr function_index;
            let open StatisticsCollector in
            cl := !cl @ [ !cognitive_complexity ]; 
            let () =
              match local_fun_name with
              | Some name ->
                increase_cognitive_complexity
                  ~fun_name:name
                  ~complexity:!cognitive_complexity
                  ~info
              | _ -> ()
            in
            ()
          | _ -> ())
    }
  in
  it.structure it parsetree;
  List.iter ~f:(printfn "%i " ) !cl;
;;
