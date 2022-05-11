open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

let run ?path_to_save parsetree info  =
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
         let () =
           try
             let open StatisticsCollector in
             let graph = CFG.build_cfg ex in
             let edges = CFG.G.nb_edges graph in
             let vertexes = CFG.G.nb_vertex graph in
             let complexity_with_cfg = edges - vertexes + 2 in
             increase_complexity ~lcomplexity:!complexity ~lcomplexity_cfg:complexity_with_cfg ~info;
             match path_to_save with
             | Some p ->
             let new_file_name = String.concat [(!info).name; ".dot"] in
             CFG.save ~new_file_name ~path_to_save:(String.concat [p; "/"]) graph;
             | None -> ();
           with
           | CFG.SomethingIsWrong -> printfn "Oops";
          in ()
       | _ -> ();
     )
  }
  in
  it.structure it parsetree
;;