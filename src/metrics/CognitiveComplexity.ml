open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

let get_logic_exp_complexity exp = 0

let rec get_complexity current_expr nesting_level =
  match current_expr.pexp_desc with
  | Pexp_ifthenelse (cond, f, s) ->
    let first_exp_compl = 1 + nesting_level + get_complexity f (nesting_level + 1) in
    let result =
      match s with
      | Some x ->
        let value = 1 + nesting_level + get_complexity x (nesting_level + 1) in
        value + first_exp_compl
      | None -> first_exp_compl
    in
    result
  | Pexp_match (exp, cases) ->
    let cases_complexity =
      List.fold
        ~f:(fun acc x -> acc + get_complexity x.pc_rhs (nesting_level + 1))
        ~init:0
        cases
    in
    cases_complexity + 1
  | Pexp_sequence (exp1, exp2) ->
    let first_exp_compl = get_complexity exp1 nesting_level in
    let second_exp_compl = get_complexity exp2 nesting_level in
    first_exp_compl + second_exp_compl
  | Pexp_let (_, _, exp) -> get_complexity exp nesting_level
  | _ -> 0
;;

let run parsetree info =
  let open CFG in
  let cfg = CFG.build_cfg parsetree in
  let result = get_complexity parsetree 0 in
  printfn "Result: %i" result;
  ()
;;
