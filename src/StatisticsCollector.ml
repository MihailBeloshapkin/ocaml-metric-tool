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
  mutable count_if_then : int;
  mutable count_of_match : int;
  mutable count_of_function : int;
  mutable count_of_let : int;
  mutable count_of_fun : int;
  mutable count_of_ident : int
};;

type complexity = {
  mutable cc_complexity : int;
  mutable branching : int
};;

type loc = {
  mutable p_loc : int;
  mutable l_loc : int
};;

type holsted_data = {
  mutable operators : string list;
  mutable operands : string list;
  mutable u_operators : string list;
  mutable u_operands : string list
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
let holsted = { operators = []; operands = []; u_operators = []; u_operands = [] }

let loc = { p_loc = 0; l_loc = 0 }

let increase_complexity br =
  compl.cc_complexity <- compl.cc_complexity + 1;
  compl.branching <- compl.branching + br 
;; 


let increase_loc () =
  loc.p_loc <- loc.p_loc + 1;
  loc.l_loc <- loc.l_loc + 1
;; 

let add_operator op = 
  holsted.operators <- op::holsted.operators;
  if not @@ List.exists ~f: (fun x -> String.equal x op) holsted.u_operators then holsted.u_operators <- op::holsted.u_operators else () 
;;

let add_operand op = 
  holsted.operands <- op::holsted.operands;
  if not @@ List.exists ~f: (fun x -> String.equal x op) holsted.u_operands then holsted.u_operands <- op::holsted.u_operands else ()
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

let calc_holsted () =
  let n1 = holsted.u_operators |> List.length |> float in
  let n2 = holsted.u_operands |> List.length |> float in
  n1 *. (Float.log n1) +. n2 *. (Float.log n2)
;;

let report () = 
  Caml.Format.printf "Operators/operands count: %d/%d\n" (List.length holsted.operators) (List.length holsted.operands);
  Caml.Format.printf "Operators: ";
  List.iter ~f: (fun x -> Caml.Format.printf "%s " x) holsted.operators;
  Caml.Format.printf "\nOperands: ";
  List.iter ~f: (fun x -> Caml.Format.printf "%s " x) holsted.operands;
  Caml.Format.printf "\nHolsted value : %f\n" (calc_holsted ());
  Caml.Format.printf "Complexity: %d\n" compl.cc_complexity;
  Caml.Format.printf "Branching: %d\n" compl.branching;
  Caml.Format.printf "Count Of if-then: %d\n" g_info.count_if_then;
  Caml.Format.printf "Count Of match: %d\n" g_info.count_of_match;
  Caml.Format.printf "Count Of let: %d\n" g_info.count_of_let;
  Caml.Format.printf "Count Of functions: %d\n" g_info.count_of_fun;
  Caml.Format.printf "Count of identifiers: %d\n" g_info.count_of_ident;
  Caml.Format.printf "Count Of function | -> ... | -> ... expression: %d\n" g_info.count_of_function;
;;