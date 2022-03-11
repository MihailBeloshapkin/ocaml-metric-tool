open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "Helsted"
let lint_source = LINT.FPCourse

type helstedType =
  | Operator
  | Operand
  | Unknown

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Calculates Helsted metric.
|}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let analyze_const = function
  | Pconst_integer (info, _) -> StatisticsCollector.add_operand info
  | Pconst_char (info) -> info |> Char.to_string |> StatisticsCollector.add_operand
  | Pconst_string (info, _, _) -> StatisticsCollector.add_operand info
  | Pconst_float (info, _) -> StatisticsCollector.add_operand info
;;

let add_ops = function
  | Pexp_ifthenelse (_, _, _) -> StatisticsCollector.add_operator "if-then" 
  | Pexp_match (_, _) -> StatisticsCollector.add_operator "match"
  | Pexp_function (_) -> StatisticsCollector.add_operator "function"
  | Pexp_for (_, _, _, _, _) -> StatisticsCollector.add_operator "for"
  | Pexp_while (_, _) -> StatisticsCollector.add_operator "while"
  | Pexp_let( _, _, _) -> StatisticsCollector.add_operator "let"
  | Pexp_constant (con) -> analyze_const con
  | _ -> ()
;;

exception SomethingIdWrong

let add_helsted_info info op_type =
  match op_type with
  | Operator -> StatisticsCollector.add_operator info
  | Operand -> StatisticsCollector.add_operand info
  | _ -> raise SomethingIdWrong
;;

let rec analyze_lident (txt : Longident.t) op_type =
  match txt with
  | Lident(info) -> add_helsted_info info op_type            
  | Ldot(t, info) -> 
    add_helsted_info info op_type;
    analyze_lident t op_type  
  | Lapply(t1, t2) -> 
    analyze_lident t1 op_type;
    analyze_lident t2 op_type
;;


let analyze_apply ex li = 
  let expr_list = li |> List.map ~f: snd in
  List.iter 
    ~f: 
    (fun expr -> 
       match expr.pexp_desc with
       | Pexp_ident { txt; _ } -> analyze_lident txt Operand
       | _ -> ())
    expr_list;

  match ex.pexp_desc with
  | Pexp_ident { txt; _ } -> 
    analyze_lident txt Operator;
  | _ -> ();
;;

let run _ fallback =
  { fallback with
    expr =
      (fun self currentExpr ->
        match currentExpr.pexp_desc with
        | Pexp_apply (ex, li) ->
          analyze_apply ex li;
          fallback.expr self currentExpr
        | _ ->
          add_ops currentExpr.pexp_desc; 
          fallback.expr self currentExpr
        )
  }
;;