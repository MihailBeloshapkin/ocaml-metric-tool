open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

let rec get_logic_exp_complexity exp =
  match exp.pexp_desc with
  | Pexp_apply (ex, li) ->
    let op =
      match ex.pexp_desc with
      | Pexp_ident { txt; _ } ->
        let result =
          match txt with
          | Lident info -> info
          | Ldot (_, info) -> info
          | _ -> ""
        in
        result
      | _ -> ""
    in
    let exprs = List.map ~f:snd li in
    let result =
      match exprs with
      | [ arg1; arg2 ] ->
        let first_result = get_logic_exp_complexity arg1 in
        let second_result = get_logic_exp_complexity arg2 in
        List.concat [ first_result; [ op ]; second_result ]
      | _ -> []
    in
    result
  | _ -> []
;;

let check_that_logic_operator_application expr =
  match expr.pexp_desc with
  | Pexp_ident { txt; _ } ->
    let result =
      match txt with
      | Lident info -> info
      | Ldot (_, info) -> info
      | _ -> ""
    in
    String.equal "&&" result || String.equal "||" result
  | _ -> false
;;

(** Process Cognitive complexity *)
let run0 parsetree info =
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
                      printfn "\nCC: %i\n" !cognitive_complexity;
                      cognitive_complexity := 1 + !nesting_level + !cognitive_complexity;
                      incr nesting_level;
                      fallback.expr self currentExpr;
                      decr nesting_level
                    | Pexp_apply (op, _) ->
                      if check_that_logic_operator_application op
                      then (
                        let res = get_logic_exp_complexity currentExpr in
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
                        cognitive_complexity := !cognitive_complexity + complexity_delta;
                        printfn "Complexity delta: %i" complexity_delta)
                      else ()
                    | _ -> fallback.expr self currentExpr)
              }
            in
            let local_it = process_function Ast_iterator.default_iterator in
            local_it.expr local_it ex;
            printf "\nCognitive complexity: %i:\n" !cognitive_complexity;
            ()
          | _ -> ())
    }
  in
  it.structure it parsetree
;;

let run parsetree info =
  let open CFG in
  let cfg = CFG.build_cfg parsetree in
  ()
;;
