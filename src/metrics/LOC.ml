open Caml
open Base
open Zanuda_core

(** Load source code from the file. *)
let get_file filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with
    | End_of_file -> None
  in
  let rec sub acc =
    match try_read () with
    | None ->
      close_in ic;
      acc |> List.rev
    | Some data -> sub (data :: acc)
  in
  let data = sub [] in
  String.concat ~sep:"\n" data
;;

(** Run LOC metric *)
let run filename info =
  let open StatisticsCollector in
  let source = get_file filename in
  let _, lines, comm_lines, llines = Lexer.process source in
  StatisticsCollector.set_loc ~lines ~lloc:llines ~comments:comm_lines ~info
;;
