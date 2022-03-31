open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "Holsted Metric"
let lint_source = LINT.FPCourse

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

type holsted_type =
  | Operator
  | Operand
  | Unknown

type program_content = {
  mutable operators : string list;
  mutable operands : string list
};;

  
exception SomethingIdWrong

let analyze_const = function
  | Pconst_integer (info, _) -> info
  | Pconst_char (info) -> info |> Char.to_string
  | Pconst_string (info, _, _) -> info
  | Pconst_float (info, _) -> info
;;

let add_ops = function
  | Pexp_ifthenelse (_, _, _) -> { operators = ["if-then"] ; operands = [] } 
  | Pexp_match (_, _)         -> { operators = ["match"]   ; operands = [] }
  | Pexp_function (_)         -> { operators = ["function"]; operands = [] }
  | Pexp_for (_, _, _, _, _)  -> { operators = ["for"]     ; operands = [] }
  | Pexp_while (_, _)         -> { operators = ["while"]   ; operands = [] }
  | Pexp_let( _, _, _)        -> { operators = ["let"]     ; operands = [] }
  | Pexp_constant (con)       -> { operators = []          ; operands = [analyze_const con] }
  | _                         -> { operators = []          ; operands = [] }                         
;;

let add_helsted_info info op_type h_data =
  match op_type with
  | Operator -> { h_data with operators = info::h_data.operators }
  | Operand -> { h_data with operands = info::h_data.operands }
  | _ -> raise SomethingIdWrong
;;

let analyze_lident (txt : Longident.t) op_type =
  (*TODO: Solve problem with long identifiers*)
  let rec loop_ident tx acc =
    match txt with
    | Lident(info) -> add_helsted_info info op_type acc     
    | Ldot(t, info) -> 
      add_helsted_info info op_type acc
    | Lapply(t1, t2) -> 
      let fst_data = loop_ident t1 acc in
      let snd_data = loop_ident t2 acc in
      { operators = (List.concat [fst_data.operators; snd_data.operators]); operands = (List.concat [fst_data.operands; snd_data.operands]) }
  in
  loop_ident txt { operators = []; operands = [] }
;;

let concat_prog_data holsted_list =
  {
    operators = holsted_list |> List.map ~f: (fun x -> x.operators) |> List.concat; 
    operands = holsted_list |> List.map ~f: (fun x -> x.operands) |> List.concat
  }
;;

let analyze_apply ex li = 
  let get_data_from_expr_list exprs =
    List.fold
      ~f: 
      (fun acc expr -> 
         match expr.pexp_desc with
         | Pexp_ident { txt; _ } -> 
           let data = analyze_lident txt Operand in
           concat_prog_data [data; acc]
         | _ -> acc)
      ~init: { operators = []; operands = [] }
      exprs
  in
  let get_data_from_expr expr =
    match expr.pexp_desc with
    | Pexp_ident { txt; _ } -> 
      analyze_lident txt Operator
    | _ -> { operators = []; operands = [] }
  in

  let data1 = li |> List.map ~f: snd |> get_data_from_expr_list in
  let data2 = get_data_from_expr ex in
  concat_prog_data [data1; data2]
;;

let run parsetree =
  let it = {
  Ast_iterator.default_iterator with
   expr = 
     (fun _ ex ->
       match ex.pexp_desc with
       | Pexp_function (_)
       | Pexp_fun (_, _, _, _) ->
         let acc : program_content ref = ref { operators = []; operands = [] } in       
         
         let process_function (l_acc : program_content ref) fallback =
           { fallback with
             expr =
               (fun self currentExpr ->
                  match currentExpr.pexp_desc with
                  | Pexp_apply (ex, li) ->
                    let data = analyze_apply ex li in
                    l_acc := concat_prog_data [!l_acc; data];
                    fallback.expr self currentExpr
                  | _ -> 
                    let data = add_ops currentExpr.pexp_desc in
                    l_acc := concat_prog_data [!l_acc; data];
                    fallback.expr self currentExpr
                )
           }
         in
         let local_it = process_function acc Ast_iterator.default_iterator in
         local_it.expr local_it ex;
         StatisticsCollector.add_holsted_for_func !acc.operators !acc.operands;
       | _ -> ()
     ) 
  }
  in
  it.structure it parsetree
;;