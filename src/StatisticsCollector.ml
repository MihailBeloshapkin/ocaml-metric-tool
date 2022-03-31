open Base
open Caml.Format
open Utils

type complexity = {
  mutable complexity          : int;
  mutable complexity_with_cfg : int
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
  mutable volume             : float;
  mutable theoretical_volume : float
};;

(* Contains info about cyclomatic complexity *)
let cc_data = ref []

let holsted = { operators = []; operands = []; unique_operators = []; unique_operands = []; volume = 0.0; theoretical_volume = 0.0 }

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
  let vol = (local_operators |> List.length |> float) +. (local_operands |> List.length |> float) in
  let theor_vol = calc_holsted ~u_operators ~u_operands in
  let new_data = 
    { operators = local_operators; 
      operands = local_operands; 
      unique_operators = u_operators; 
      unique_operands = u_operands; 
      volume = vol; 
      theoretical_volume = theor_vol 
      } 
    in
  holsted_for_functions := new_data::!holsted_for_functions
;;

let increase_complexity ~lcomplexity ~lcomplexity_cfg =
  cc_data := { complexity = lcomplexity; complexity_with_cfg = lcomplexity_cfg }::!cc_data;
;; 

exception SomethingWrong

let set_loc ~lines ~lloc ~comments = 
  loc_metric.lines <- lines;
  loc_metric.lloc <- lloc;
  loc_metric.comments <- comments
;;

let report_loc () =
  Caml.Format.printf "\nLOC:\n";
  Caml.Format.printf "  Lines: %d\n" loc_metric.lines;
  Caml.Format.printf "  Logical lines: %d\n" loc_metric.lloc;
  Caml.Format.printf "  Comments: %d\n" loc_metric.comments;
;;

let report_holsted () =
  !holsted_for_functions 
  |> List.rev 
  |> List.iteri
    ~f: 
    (fun i x ->
      printf "func: %d\n" i;
      printf "  Operators: ";
      List.iter ~f:(fun l_operator -> printf "%s " l_operator) x.operators;
      print_string "\n  Operands: ";
      List.iter ~f:(fun l_operand -> printf "%s " l_operand) x.operands;
      print_string "\n  Unique Operators: ";
      List.iter ~f:(fun l_operator -> printf "%s " l_operator) x.unique_operators;
      print_string "\n  Unique Operands: ";
      List.iter ~f:(fun l_operand -> printf "%s " l_operand) x.unique_operands;
      printf "\n  Volume: %f Theoretical volume: %f \n\n" x.volume x.theoretical_volume;
    )
;;

let report_cc () =
  !cc_data
  |> List.rev
  |> List.iteri
    ~f:
    (fun i x ->
      printf "\nfunc: %d\n" i;
      printf "Cyclomatic complexity:\n";
      printf "  without CFG: %i\n  with CFG: %i\n\n" x.complexity x.complexity_with_cfg;
    )
;;

let report = function 
  | "loc"      -> report_loc ();
  | "halstead" -> report_holsted ();
  | "cc"       -> report_cc ();
  | _          -> ();
;;