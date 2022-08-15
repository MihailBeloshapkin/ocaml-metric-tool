open Base
open Caml.Format
open Utils

type complexity =
  { mutable f_name : string
  ; mutable complexity : int
  ; mutable complexity_with_cfg : int
  }

type cognitive_complexity =
  { mutable func_name : string
  ; mutable cogn_complexity : int
  }

type loc =
  { mutable lines : int
  ; mutable lloc : int
  ; mutable comments : int
  }

type holsted_data =
  { mutable operators : string list
  ; mutable operands : string list
  ; mutable unique_operators : string list
  ; mutable unique_operands : string list
  ; mutable volume : float
  ; mutable theoretical_volume : float
  }

let holsted =
  { operators = []
  ; operands = []
  ; unique_operators = []
  ; unique_operands = []
  ; volume = 0.0
  ; theoretical_volume = 0.0
  }
;;

type module_info =
  { name : string
  ; mutable cc_data : complexity list option
  ; mutable cogn_compl_data : cognitive_complexity list option
  ; mutable holsted_for_funcs : holsted_data list option
  ; mutable loc_metric : loc option
  }

let distinct_list li =
  li
  |> List.fold
       ~f:(fun acc x ->
         if not @@ List.exists ~f:(fun el -> String.equal el x) acc then x :: acc else acc)
       ~init:[]
;;

let common_data : module_info list ref = ref []

let calc_holsted ~u_operators ~u_operands =
  let n1 = u_operators |> List.length |> float in
  let n2 = u_operands |> List.length |> float in
  (n1 *. Float.log n1) +. (n2 *. Float.log n2)
;;

let add_holsted_for_func
    (local_operators : string list)
    (local_operands : string list)
    ~info
  =
  let u_operators = distinct_list local_operators in
  let u_operands = distinct_list local_operands in
  let vol =
    (local_operators |> List.length |> float) +. (local_operands |> List.length |> float)
  in
  let theor_vol = calc_holsted ~u_operators ~u_operands in
  let new_data =
    { operators = local_operators
    ; operands = local_operands
    ; unique_operators = u_operators
    ; unique_operands = u_operands
    ; volume = vol
    ; theoretical_volume = theor_vol
    }
  in
  info
    := { !info with
         holsted_for_funcs =
           (match !info.holsted_for_funcs with
           | Some data -> Some (new_data :: data)
           | None -> Some [ new_data ])
       }
;;

let increase_complexity fun_name ~lcomplexity ~lcomplexity_cfg ~info =
  let new_data =
    { f_name = fun_name; complexity = lcomplexity; complexity_with_cfg = lcomplexity_cfg }
  in
  info
    := { !info with
         cc_data =
           (match !info.cc_data with
           | Some data -> Some (new_data :: data)
           | None -> Some [ new_data ])
       }
;;

let increase_cognitive_complexity ~fun_name ~complexity ~info =
  let new_data = { func_name = fun_name; cogn_complexity = complexity } in
  info
    := { !info with
         cogn_compl_data =
           (match !info.cogn_compl_data with
           | Some data -> Some (new_data :: data)
           | None -> Some [ new_data ])
       };
  ()
;;

exception SomethingWrong

let set_loc ~lines ~lloc ~comments ~info =
  let loc_metric = { lines; lloc; comments } in
  info := { !info with loc_metric = Some loc_metric }
;;

let add_module_info info = common_data := info :: !common_data

let report_loc loc_metric =
  Caml.Format.printf "\n  LOC:\n";
  Caml.Format.printf "    Lines: %d\n" loc_metric.lines;
  Caml.Format.printf "    Logical lines: %d\n" loc_metric.lloc;
  Caml.Format.printf "    Comments: %d\n" loc_metric.comments
;;

let report_holsted holsted_for_functions =
  holsted_for_functions
  |> List.rev
  |> List.iteri ~f:(fun i x ->
         printf "func: %d\n" i;
         printf "  Operators: ";
         List.iter ~f:(fun l_operator -> printf "%s " l_operator) x.operators;
         print_string "\n  Operands: ";
         List.iter ~f:(fun l_operand -> printf "%s " l_operand) x.operands;
         print_string "\n  Unique Operators: ";
         List.iter ~f:(fun l_operator -> printf "%s " l_operator) x.unique_operators;
         print_string "\n  Unique Operands: ";
         List.iter ~f:(fun l_operand -> printf "%s " l_operand) x.unique_operands;
         printf "\n  Volume: %f Theoretical volume: %f \n\n" x.volume x.theoretical_volume)
;;

let report_cc cc_data =
  cc_data
  |> List.rev
  |> List.iter ~f:(fun x ->
         printf "\n  func: %s\n" x.f_name;
         printf "  Cyclomatic complexity:\n";
         printf
           "    without CFG: %i\n    with CFG: %i\n\n"
           x.complexity
           x.complexity_with_cfg)
;;

let report_cg cg_data =
  cg_data
  |> List.rev
  |> List.iter ~f:(fun x ->
         printf "\n  func: %s" x.func_name;
         printf "\n  Cognitive complexity: %i\n" x.cogn_complexity)
;;

let report_all () =
  !common_data
  |> List.iter ~f:(fun x ->
         printfn "\nModule: %s\n" x.name;
         Option.iter x.loc_metric ~f:report_loc;
         Option.iter x.cc_data ~f:report_cc;
         Option.iter x.cogn_compl_data ~f:report_cg;
         Option.iter x.holsted_for_funcs ~f:report_holsted)
;;
