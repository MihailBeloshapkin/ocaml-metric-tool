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


let process_function e =
  let it = 
    { Ast_iterator.default_iterator with
      expr =
      (fun self currentExpr ->
        Caml.Format.printf "\n======================\n";
        Pprintast.expression Format.std_formatter currentExpr;
        self.expr self currentExpr;
    )
    }
  in 
  it.expr it e
;;

let a_pat = function
  | Ppat_var { txt; _} -> Caml.Format.printf "======INFO:%s========\n" txt;
  | _ -> Caml.Format.printf "==============\n";
;;

let a_ex = function
  | Pexp_ident (_) -> Caml.Format.printf "======Ident========\n";
  | _ -> Caml.Format.printf "======NoIdent========\n";
;;

type helstedType =
  | Operator
  | Operand
  | Unknown


type holsted_data = {
  mutable operators : string list;
  mutable operands : string list
};;

  
exception SomethingIdWrong

let analyze_const acc = function
  | Pconst_integer (info, _) -> { acc with operands = info::acc.operands }
  | Pconst_char (info) -> { acc with operands = (info |> Char.to_string)::acc.operands }
  | Pconst_string (info, _, _) -> { acc with operands = info::acc.operands }
  | Pconst_float (info, _) -> { acc with operands = info::acc.operands }
;;

let add_ops acc = function
  | Pexp_ifthenelse (_, _, _) -> { acc with operators = "if-then"::acc.operators } 
  | Pexp_match (_, _) -> { acc with operators = "match"::acc.operators }
  | Pexp_function (_) -> { acc with operators = "function"::acc.operators }
  | Pexp_for (_, _, _, _, _) -> { acc with operators = "for"::acc.operators }
  | Pexp_while (_, _) -> { acc with operators = "while"::acc.operators }
  | Pexp_let( _, _, _) -> { acc with operators = "let"::acc.operators }
  | Pexp_constant (con) -> analyze_const acc con
  | _ -> acc
;;

let add_helsted_info info op_type h_data =
  match op_type with
  | Operator -> { h_data with operators = info::h_data.operators }
  | Operand -> { h_data with operands = info::h_data.operands }
  | _ -> raise SomethingIdWrong
;;


let analyze_lident (txt : Longident.t) op_type =
  let rec loop_ident tx acc =
    match txt with
    | Lident(info) -> add_helsted_info info op_type acc     
    | Ldot(t, info) -> 
      loop_ident t (add_helsted_info info op_type acc)
    | Lapply(t1, t2) -> 
      let fst_data = loop_ident t1 acc in
      let snd_data = loop_ident t2 acc in
      { operators = (List.concat [fst_data.operators; snd_data.operators]); operands = (List.concat [fst_data.operands; snd_data.operands]) }
  in
  loop_ident txt { operators = []; operands = [] }
;;


let concat_holsted holsted_list =
  {
    operators = holsted_list |> List.map ~f: (fun x -> x.operators) |> List.concat; 
    operands = holsted_list |> List.map ~f: (fun x -> x.operands) |> List.concat
  }
;;

let analyze_apply ex li = 
  let expr_list = li |> List.map ~f: snd in
  let get_li exprs =
    List.fold 
      ~f: 
      (fun acc expr -> 
         match expr.pexp_desc with
         | Pexp_ident { txt; _ } -> 
           let data = analyze_lident txt Operand in
           concat_holsted [data; acc]
         | _ -> acc)
      ~init: { operators = []; operands = [] }
      exprs
  in
  let get_ex expr =
    match expr.pexp_desc with
    | Pexp_ident { txt; _ } -> 
      analyze_lident txt Operator
    | _ -> { operators = []; operands = [] }
  in

  let data1 = li |> List.map ~f: snd |> get_li in
  let data2 = get_ex ex in
  concat_holsted [data1; data2]
;;

let run info parsetree =
  let acc : holsted_data list = [] in
  let it = {
  Ast_iterator.default_iterator with
   expr = 
     (fun self ex ->
       match ex.pexp_desc with
       | Pexp_fun (lab, _, pat, _) ->
         Caml.Format.printf "======Function========\n";
         let acc : holsted_data ref = ref {operators = []; operands = []} in       
         let l_run (l_acc : holsted_data ref) fallback =
           { fallback with
            expr =
             (fun self currentExpr ->
                let loc = currentExpr.pexp_loc in
                let filename = loc.Location.loc_start.Lexing.pos_fname in
                match currentExpr.pexp_desc with
                | Pexp_apply (ex, li) ->
                  let data = analyze_apply ex li in
                  (* List.iter ~f: (fun c -> Caml.Format.printf "Operator:%s  " c) data.operators;
                  List.iter ~f: (fun c -> Caml.Format.printf "OPerand: %s  " c) data.operands; *)
                  l_acc := concat_holsted [!l_acc; data];
                  fallback.expr self currentExpr 
                | _ -> 
                  (*List.iter ~f: (fun c -> Caml.Format.printf "1Operator:%s  " c) data.operators;
                  List.iter ~f: (fun c -> Caml.Format.printf "1OPerand: %s  " c) data.operands; *)
                  let data = add_ops !l_acc currentExpr.pexp_desc in
                  l_acc := add_ops !l_acc currentExpr.pexp_desc;
                  fallback.expr self currentExpr
              )
         }
         in
         Caml.Format.printf "Point\n";
         let lit = l_run acc Ast_iterator.default_iterator in
         lit.expr lit ex;
         List.iter ~f: (fun c -> Caml.Format.printf "Operator:%s  " c) !acc.operators;
         List.iter ~f: (fun c -> Caml.Format.printf "OPerand: %s  " c) !acc.operands;
         
        (* let loc_it = l_run Ast_iterator.default_iterator ex*)   
       | _ -> Caml.Format.printf "======Other========\n";
     )
  }
  in 
  it.structure it parsetree
;;