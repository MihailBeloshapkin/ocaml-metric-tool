open Base
open Caml.Format
open Utils

type variants =
  | LetExp
  | IfThenElseExp
  | MatchExp
  | FunExp
  | FunctionExp
  | IdentExp

type general_info = {
  mutable count_if_then     : int;
  mutable count_of_match    : int;
  mutable count_of_function : int;
  mutable count_of_let      : int;
  mutable count_of_fun      : int;
  mutable count_of_ident    : int
};;

type complexity = {
  mutable cc_complexity : int;
  mutable branching     : int
};;

type loc = {
  mutable lines     : int;
  mutable lloc      : int;
  mutable comments  : int
};;

type holsted_data = {
  mutable operators          : string list;
  mutable operands           : string list;
  mutable unique_operators   : string list;
  mutable unique_operands    : string list;
  mutable holsted_coeff      : float
};;

let g_info = { 
  count_if_then = 0; 
  count_of_match = 0; 
  count_of_function = 0; 
  count_of_let = 0; 
  count_of_fun = 0; 
  count_of_ident = 0
}

let compl = { cc_complexity = 1; branching = 0 }
let holsted = { operators = []; operands = []; unique_operators = []; unique_operands = []; holsted_coeff = 0.0 }

let loc_metric = { lines = 0; lloc = 0 ; comments = 0}

let holsted_for_functions : holsted_data list ref = ref [] 

let distinct_list li = li |> List.fold ~f:(fun acc x -> if not @@ List.exists ~f:(fun el -> String.equal el x) acc then x::acc else acc) ~init:[]

let calc_holsted ~u_operators ~u_operands =
  let n1 = u_operators |> List.length |> float in
  let n2 = u_operands |> List.length |> float in
  n1 *. (Float.log n1) +. n2 *. (Float.log n2)
;;

let add_holsted_for_func (local_operators : string list) (local_operands : string list) =
  let u_operators = distinct_list local_operators in
  let u_operands = distinct_list local_operands in
  let holsted_coeff = calc_holsted ~u_operators ~u_operands in
  let new_data = { operators = local_operators; operands = local_operands; unique_operators = u_operators; unique_operands = u_operands; holsted_coeff } in
  holsted_for_functions := new_data::!holsted_for_functions
;;

let increase_complexity br =
  compl.cc_complexity <- compl.cc_complexity + 1;
  compl.branching <- compl.branching + br 
;; 

exception SomethingWrong

let add = function
  | LetExp -> g_info.count_of_let <- g_info.count_of_let + 1
  | IfThenElseExp -> g_info.count_if_then <- g_info.count_if_then + 1
  | MatchExp -> g_info.count_of_match <- g_info.count_of_match + 1
  | FunExp -> g_info.count_of_fun <- g_info.count_of_fun + 1
  | FunctionExp -> g_info.count_of_function <- g_info.count_of_function + 1
  | IdentExp -> g_info.count_of_ident <- g_info.count_of_ident + 1  
  (*| _ -> raise SomethingWrong *)
;;

let set_loc ~lines ~lloc ~comments = 
  loc_metric.lines <- lines;
  loc_metric.lloc <- lloc;
  loc_metric.comments <- comments
;;

let report_loc () =
  Caml.Format.printf "LOC:\n";
  Caml.Format.printf "  Lines: %d\n" loc_metric.lines;
  Caml.Format.printf "  Logical lines: %d\n" loc_metric.lloc;
  Caml.Format.printf "  Comments: %d\n" loc_metric.comments;
;;

let report_holsted () =
  List.iteri 
    ~f: 
    (fun i x ->
      Caml.Format.printf "func: %d\n" i;
      Caml.Format.printf "  Operators: ";
      List.iter ~f:(fun l_operator -> Caml.Format.printf "%s " l_operator) x.operators;
      print_string "\n  Operands: ";
      List.iter ~f:(fun l_operand -> Caml.Format.printf "%s " l_operand) x.operands;
      print_string "\n  Unique Operators: ";
      List.iter ~f:(fun l_operator -> Caml.Format.printf "%s " l_operator) x.unique_operators;
      print_string "\n  Unique Operands: ";
      List.iter ~f:(fun l_operand -> Caml.Format.printf "%s " l_operand) x.unique_operands;
      Caml.Format.printf "\n  Coefficent: %f\n\n" x.holsted_coeff;
      )
    !holsted_for_functions
;;

let report () =
  report_loc ();
  report_holsted ();
  (*Caml.Format.printf "Complexity: %d\n" compl.cc_complexity;
  Caml.Format.printf "Branching: %d\n" compl.branching;
  Caml.Format.printf "Count Of if-then: %d\n" g_info.count_if_then;
  Caml.Format.printf "Count Of match: %d\n" g_info.count_of_match;
  Caml.Format.printf "Count Of let: %d\n" g_info.count_of_let;
  Caml.Format.printf "Count Of functions: %d\n" g_info.count_of_fun;
  Caml.Format.printf "Count of identifiers: %d\n" g_info.count_of_ident;
  Caml.Format.printf "Count Of function | -> ... | -> ... expression: %d\n" g_info.count_of_function;
*)
;;