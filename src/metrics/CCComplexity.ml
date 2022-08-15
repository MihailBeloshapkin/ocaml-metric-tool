open Base
open Caml.Format
open Zanuda_core
open Utils
open Parsetree
open Ast_iterator

let get_fun_names input =
  let exprs = ref [] in
  let pats = ref [] in
  let it =
    { Ast_iterator.default_iterator with
      expr = (fun _ ex -> exprs := !exprs @ [ ex ])
    ; pat = (fun _ pat -> pats := !pats @ [ pat ])
    }
  in
  it.structure it input;
  match List.zip !exprs !pats with
  | Ok li ->
    li
    |> List.filter ~f:(fun (e, _) ->
           match e.pexp_desc with
           | Pexp_fun _ | Pexp_function _ -> true
           | _ -> false)
    |> List.map ~f:snd
    |> List.map ~f:(fun p ->
           match p.ppat_desc with
           | Ppat_var { txt } -> txt
           | _ -> "")
  | _ -> []
;;

let run ?path_to_save parsetree info =
  let fun_names = get_fun_names parsetree in
  let function_index = ref 0 in
  let it =
    { Ast_iterator.default_iterator with
      expr =
        (fun _ ex ->
          match ex.pexp_desc with
          | Pexp_function _ | Pexp_fun (_, _, _, _) ->
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
                      fallback.expr self currentExpr
                    | _ -> fallback.expr self currentExpr)
              }
            in
            let local_it = process_function Ast_iterator.default_iterator in
            local_it.expr local_it ex;
            let name = List.nth fun_names !function_index in
            let () =
              try
                let open StatisticsCollector in
                let graph = CFG.build_cfg ex in
                let edges = CFG.G.nb_edges graph in
                let vertexes = CFG.G.nb_vertex graph in
                let complexity_with_cfg = edges - vertexes + 2 in
                let fix_results fun_name =
                  increase_complexity
                    fun_name
                    ~lcomplexity:!complexity
                    ~lcomplexity_cfg:complexity_with_cfg
                    ~info;
                  match path_to_save with
                  | Some p ->
                    let new_file_name =
                      String.concat [ !info.name; "_"; fun_name; ".dot" ]
                    in
                    CFG.save ~new_file_name ~path_to_save:(String.concat [ p; "/" ]) graph;
                    printf "\nSaved\n"
                  | None -> ()
                in
                Option.iter ~f:fix_results name
              with
              | CFG.CfgBuildFailed ->
                Option.iter ~f:(printfn "Oops: Module:%s Func:%s\n" !info.name) name
            in
            incr function_index
          | _ -> ())
    }
  in
  it.structure it parsetree
;;
