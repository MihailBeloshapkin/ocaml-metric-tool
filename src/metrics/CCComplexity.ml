open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "Other Stat"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Calculates number of different kinds of expressions.
|}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let run fallback =
  { fallback with
    expr =
      (fun self currentExpr ->
         match currentExpr.pexp_desc with
         | Pexp_while (_, _)
         | Pexp_for (_, _, _, _, _)
         | Pexp_ifthenelse (_, _, _) ->
           StatisticsCollector.increase_complexity 1;
         | Pexp_function (li)
         | Pexp_match (_, li)
         | Pexp_try (_, li) -> 
           let delta = (List.length li) - 1 in
           StatisticsCollector.increase_complexity delta;
         | _ -> ();
         fallback.expr self currentExpr
      )
  }
;;