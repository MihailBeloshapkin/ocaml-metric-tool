open Base
open Caml.Format
open Utils

module CyclComplexity = struct
  type t =
    { mutable func_name : string
    ; mutable complexity : int
    ; mutable complexity_with_cfg : int
    }
end

module CognComplexity = struct
  type t =
    { mutable func_name : string
    ; mutable cogn_complexity : int
    }
end

module Loc = struct
  type t =
    { mutable lines : int
    ; mutable lloc : int
    ; mutable comments : int
    }
end

module HolsedData = struct
  type t =
    { func_name : string
    ; mutable operators : string list
    ; mutable operands : string list
    ; mutable unique_operators : string list
    ; mutable unique_operands : string list
    ; mutable volume : float
    ; mutable theoretical_volume : float
    }
end

(*
let holsted =
  { HolsedData.operators = []
  ; HolsedData.operands = []
  ; HolsedData.unique_operators = []
  ; HolsedData.unique_operands = []
  ; HolsedData.volume = 0.0
  ; HolsedData.theoretical_volume = 0.0
  }
;;
*)

module ModuleInfo = struct
  type t =
    { name : string
    ; cycl_compl_data : CyclComplexity.t list option
    ; cogn_compl_data : CognComplexity.t list option
    ; holsted_for_funcs : HolsedData.t list option
    ; loc_metric : Loc.t option
    }
end

let distinct_list li =
  li
  |> List.fold
       ~f:(fun acc x ->
         if not @@ List.exists ~f:(fun el -> String.equal el x) acc then x :: acc else acc)
       ~init:[]
;;

let common_data : ModuleInfo.t list ref = ref []

let calc_holsted ~u_operators ~u_operands =
  let n1 = u_operators |> List.length |> float in
  let n2 = u_operands |> List.length |> float in
  (n1 *. Float.log n1) +. (n2 *. Float.log n2)
;;

let add_holsted_for_func
  fun_name
  (local_operators : string list)
  (local_operands : string list)
  ~info
  =
  let open HolsedData in
  let u_operators = distinct_list local_operators in
  let u_operands = distinct_list local_operands in
  let vol =
    (local_operators |> List.length |> float) +. (local_operands |> List.length |> float)
  in
  let theor_vol = calc_holsted ~u_operators ~u_operands in
  let new_data =
    { func_name = fun_name
    ; operators = local_operators
    ; operands = local_operands
    ; unique_operators = u_operators
    ; unique_operands = u_operands
    ; volume = vol
    ; theoretical_volume = theor_vol
    }
  in
  let open ModuleInfo in
  info
    := { !info with
         holsted_for_funcs =
           (match !info.holsted_for_funcs with
            | Some data -> Some (new_data :: data)
            | None -> Some [ new_data ])
       }
;;

let increase_complexity fun_name ~lcomplexity ~lcomplexity_cfg ~info =
  let open CyclComplexity in
  let open ModuleInfo in
  let new_data =
    { func_name = fun_name
    ; complexity = lcomplexity
    ; complexity_with_cfg = lcomplexity_cfg
    }
  in
  info
    := { !info with
         cycl_compl_data =
           (match !info.cycl_compl_data with
            | Some data -> Some (new_data :: data)
            | None -> Some [ new_data ])
       }
;;

let increase_cognitive_complexity ~fun_name ~complexity ~info =
  let open CognComplexity in
  let open ModuleInfo in
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
  let open Loc in
  let loc_metric = { lines; lloc; comments } in
  info := { !info with ModuleInfo.loc_metric = Some loc_metric }
;;

let add_module_info info = common_data := info :: !common_data

let report_loc loc_metric =
  let open Loc in
  Caml.Format.printf "\n  LOC:\n";
  Caml.Format.printf "    Lines: %d\n" loc_metric.lines;
  Caml.Format.printf "    Logical lines: %d\n" loc_metric.lloc;
  Caml.Format.printf "    Comments: %d\n" loc_metric.comments
;;

let report_holsted holsted_for_functions =
  let open HolsedData in
  holsted_for_functions
  |> List.rev
  |> List.iter ~f:(fun x ->
       printf "func: %s\n" x.func_name;
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
  let open CyclComplexity in
  cc_data
  |> List.rev
  |> List.iter ~f:(fun x ->
       printf "\n  func: %s\n" x.func_name;
       printf "  Cyclomatic complexity:\n";
       printf
         "    without CFG: %i\n    with CFG: %i\n\n"
         x.complexity
         x.complexity_with_cfg)
;;

let report_cg cg_data =
  let open CognComplexity in
  cg_data
  |> List.rev
  |> List.iter ~f:(fun x ->
       printf "\n  func: %s" x.func_name;
       printf "\n  Cognitive complexity: %i\n" x.cogn_complexity)
;;

let report_all () =
  let open ModuleInfo in
  !common_data
  |> List.iter ~f:(fun x ->
       printfn "\nModule: %s\n" x.name;
       Option.iter x.loc_metric ~f:report_loc;
       Option.iter x.cycl_compl_data ~f:report_cc;
       Option.iter x.cogn_compl_data ~f:report_cg;
       Option.iter x.holsted_for_funcs ~f:report_holsted)
;;
