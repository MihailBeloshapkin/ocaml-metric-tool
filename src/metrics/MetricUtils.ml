open Base
open Xmlm
open Parsetree

let get_name pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> ""
;;
