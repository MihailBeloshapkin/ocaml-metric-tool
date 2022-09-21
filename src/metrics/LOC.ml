open Caml
open Base
open Zanuda_core

(** Load source code from the file. *)
let get_file filename =
  let current_channel = open_in filename in
  let data = really_input_string current_channel (in_channel_length current_channel) in
  close_in current_channel;
  data
;;

(** Run LOC metric *)
let run filename info =
  let open StatisticsCollector in
  let source = get_file filename in
  let _, lines, comm_lines, llines = Lexer.process source in
  StatisticsCollector.set_loc ~lines ~lloc:llines ~comments:comm_lines ~info
;;
