open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "ifThenElseCount"
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
        let loc = currentExpr.pexp_loc in
        let filename = loc.Location.loc_start.Lexing.pos_fname in
        
        Caml.Format.printf "\n======================\n";
        Pprintast.expression Format.std_formatter currentExpr;

        match currentExpr.pexp_desc with
        | Pexp_let (_, vb, ex) ->
          Caml.Format.printf "Let\n";  
          self.expr self ex;
          List.iter ~f: (fun bind -> self.expr self bind.pvb_expr) vb;
          StatisticsCollector.add LetExp
        | Pexp_ifthenelse (_, _, _) ->
          Caml.Format.printf "IfThenElse\n";  
          fallback.expr self currentExpr;
          StatisticsCollector.add IfThenElseExp;
        | Pexp_match (ex, li) -> 
          Caml.Format.printf "Match";
          StatisticsCollector.add MatchExp; 
          fallback.expr self ex;
        List.iter ~f: (fun c -> self.expr self c.pc_rhs) li
        | Pexp_ident { txt; _ } ->
          StatisticsCollector.add IdentExp;
          Caml.Format.printf "\nIdent\n";
          fallback.expr self currentExpr                          
        | Pexp_constant (con) -> Caml.Format.print_string "\nConstant\n" 
        | Pexp_apply (_, _) -> 
          Caml.Format.print_string "\nApply\n";
          fallback.expr self currentExpr 
        | Pexp_unreachable -> Caml.Format.printf "Unreach"
        | Pexp_function (_) -> 
          StatisticsCollector.add FunctionExp;
          Caml.Format.print_string "\nfunction\n";
          fallback.expr self currentExpr 
        | Pexp_fun (_, _, _, _) ->
          Caml.Format.print_string "\nFun\n"; 
          StatisticsCollector.add FunExp;
          fallback.expr self currentExpr 
        | Pexp_tuple li -> 
          Caml.Format.print_string "\nTuple\n"; 
        | Pexp_array li -> 
          Caml.Format.print_string "\nArray\n"; 
        | Pexp_sequence (_, _) -> 
          Caml.Format.print_string "\nSequence\n";
          fallback.expr self currentExpr;
        | _ ->
          Caml.Format.printf "\nOther\n"; 
          fallback.expr self currentExpr)
  }
;;