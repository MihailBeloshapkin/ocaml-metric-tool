open Caml
open Base

let get_file filename =
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec sub acc =
    match try_read () with
    | None -> 
      close_in ic;
      acc |> List.rev
    | Some data -> sub (data::acc)
  in
  let data = sub [] in
  String.concat ~sep:"\n" data
;;

let run filename =
  let source = get_file filename in
  let (emply, lines, chars, comments) = Lexer.process source in
  List.iter ~f:(fun x -> (Caml.Format.printf "%s" x)) comments;
  Caml.Format.printf "\nEmpty lines: %d\n" emply; 
  () 
;; 
