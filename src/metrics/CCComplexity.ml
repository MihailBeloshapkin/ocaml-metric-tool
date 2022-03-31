open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

let run parsetree =
  let it = {
  Ast_iterator.default_iterator with
   expr = 
     (fun _ ex ->
       match ex.pexp_desc with
       | Pexp_function (_)
       | Pexp_fun (_, _, _, _) ->
         let complexity = ref 1 in
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
                 incr complexity;
                 fallback.expr self currentExpr;
                 | _ -> fallback.expr self currentExpr;
                )
           }
         in
         let local_it = process_function Ast_iterator.default_iterator in
         local_it.expr local_it ex;

         let vertexes, edges = CFG.build_cfg ex in
         let complexity_with_cfg =(List.length edges) - (List.length vertexes) + 2 in
         StatisticsCollector.increase_complexity ~lcomplexity:!complexity ~lcomplexity_cfg:complexity_with_cfg;
         ()
       | _ -> ();
     ) 
  }
  in
  it.structure it parsetree
;;