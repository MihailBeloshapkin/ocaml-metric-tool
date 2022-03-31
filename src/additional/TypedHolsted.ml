open Base
open Caml.Format
open Zanuda_core
open Utils
open Tast_iterator
open Typedtree

type holsted_type =
  | Operator
  | Operand
  
type program_content = {
  mutable operators : string list;
  mutable operands : string list
};;

let concat_prog_data holsted_list =
  {
    operators = holsted_list |> List.map ~f: (fun x -> x.operators) |> List.concat; 
    operands = holsted_list |> List.map ~f: (fun x -> x.operands) |> List.concat
  }
;;

let get_ident_name txt =
  let open Longident in
  match txt with
  | Lident(info) -> info     
  | Ldot(t, info) -> info
  | Lapply(t1, t2) -> "NoName"
;;

let analyze_apply ex (li : ('a * expression option) list) =
  let data_from_list = 
    li
    |> List.map ~f: (snd)
    |> List.map ~f: (Option.to_list)
    |> List.concat
    |> List.fold 
       ~f:(fun acc x -> 
            match x.exp_desc with
            | Texp_ident (_, { txt }, _) -> { acc with operands = (get_ident_name txt)::acc.operands}
            | _ -> acc
          ) 
        ~init: { operators = []; operands = [] }
  in
  match ex.exp_desc with
  | Texp_ident (_, { txt }, _) -> { data_from_list with operators = (get_ident_name txt)::data_from_list.operators } 
  | _ -> data_from_list
;;

let analyze_match ({ c_lhs; c_guard; c_rhs } : computation case) =
  ()
;;

let run typedtree =
  Printtyped.implementation Format.std_formatter typedtree;
  let it = {
    Tast_iterator.default_iterator with
      expr =
        (fun _ exp ->
          match exp.exp_desc with
          | Texp_function _ ->
            let acc : program_content ref = ref { operators = []; operands = [] } in
            
            let process_function (l_acc : program_content ref) fallback =
              { fallback with
                expr =
                  (fun self current_ex ->
                    match current_ex.exp_desc with
                    | Texp_function { param } -> 
                      printf "Ident: %s\n" (Ident.name param);
                      fallback.expr self current_ex; 
                    | Texp_match (ex, head::exlist, _) -> 
                      analyze_match head;
                      ();                  
                    | Texp_apply (ex, li) ->
                      printf "\nAPPLY\n";
                      let data = analyze_apply ex li in
                      l_acc := concat_prog_data [data; !l_acc];
                      printf "\nOperators:\n";
                      List.iter ~f: (fun x -> print_string x) data.operators;
                      printf "\nOperands:\n";
                      List.iter ~f: (fun x -> print_string x) data.operands;
                      fallback.expr self current_ex;
                    | _ -> fallback.expr self current_ex;
                  );
              }
            in
            let local_it = process_function acc Tast_iterator.default_iterator in
            local_it.expr local_it exp;
          | _ -> () 
        )
  }
  in
  it.structure it typedtree
;;