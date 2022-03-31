open Base
open Caml.Format
open Zanuda_core
open Utils
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
        | Pexp_ident (_) -> 
          printf "\nIdent\n";
          fallback.expr self currentExpr
        | Pexp_constant (_) -> 
          printf "\nConstant\n";
          fallback.expr self currentExpr
        | Pexp_let (_, _, _) -> 
          printf "\nLet\n";
          fallback.expr self currentExpr
        | Pexp_function (_) -> 
          printf "\nfunction\n";
          fallback.expr self currentExpr   
        | Pexp_fun (_, _, _, _) -> 
          printf "\nFun\n";
          fallback.expr self currentExpr
        | Pexp_apply (_, _) -> 
          printf "\nApply\n";
          fallback.expr self currentExpr
        | Pexp_match (_, _) -> 
          printf "\nMatch\n";
          fallback.expr self currentExpr
        | Pexp_try (_, _) ->
           printf "\nTry\n";
           fallback.expr self currentExpr
        | Pexp_tuple (_) -> 
          printf "\nTuple\n";
          fallback.expr self currentExpr
        | Pexp_construct (_, _) -> 
          printf "\nConstruct\n";
          fallback.expr self currentExpr
        | Pexp_variant (_, _) -> 
          printf "\nVariant\n";
          fallback.expr self currentExpr
        | Pexp_record (_, _) -> 
          printf "\nRecors\n";
          fallback.expr self currentExpr
        | Pexp_field (_, _) -> 
          printf "\nField\n";
          fallback.expr self currentExpr
        | Pexp_setfield (_, _, _) -> 
          printf "\nSet Field\n";
          fallback.expr self currentExpr
        | Pexp_array (_) -> 
          printf "\nArray\n";
          fallback.expr self currentExpr
        | Pexp_ifthenelse (_, _, _) -> 
          printf "\nIfThenElse\n";
          fallback.expr self currentExpr
        | Pexp_sequence (_, _) -> 
          printf "\nSequence\n";
          fallback.expr self currentExpr
        | Pexp_while (_, _) -> 
          printf "\nWhile\n";
          fallback.expr self currentExpr
        | _ -> 
          Caml.Format.printf "\nOther\n"; 
          fallback.expr self currentExpr)
  }