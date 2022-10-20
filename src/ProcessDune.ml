open Base
open Caml.Format
open Utils

type module_ =
  { name : string
  ; impl : string option
  ; intf : string option
  ; cmt : string option
  ; cmti : string option
  }
[@@deriving sexp]

type executables =
  { names : string list
  ; modules : module_ list
  ; requires : string list
  ; include_dirs : string list
  }
[@@deriving sexp]

module Library = struct
  type t =
    { name : string
    ; uid : string
    ; local : bool
    ; requires : string list
    ; source_dir : string
    ; modules : module_ list
    ; include_dirs : string list
    }
  [@@deriving sexp]
end

type t =
  | Executables of executables
  | Library of Library.t
[@@deriving sexp]

let fine_module { impl } =
  match impl with
  | Some s when String.is_suffix s ~suffix:".ml-gen" -> false
  | _ -> true
;;

let analyze_directory path analyzer =
  Unix.chdir path;
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    Caml.close_in ch;
    s
  in
  let db = [%of_sexp: t list] s in
  let get_library name =
    List.find_map db ~f:(function
      | Library l when String.equal name l.uid -> Some l
      | _ -> None)
  in
  let on_module _ m (info : StatisticsCollector.ModuleInfo.t ref) =
    (* we analyze syntax tree without expanding syntax extensions *)
    Option.iter m.impl ~f:(analyzer info);
    StatisticsCollector.add_module_info !info
  in
  let loop_database () =
    let open StatisticsCollector in
    let open StatisticsCollector.ModuleInfo in
    List.iter db ~f:(function
      | Executables { modules; requires } ->
        ()
        (*let extra_paths =
            requires
            |> List.filter_map ~f:(fun uid -> get_library uid)
            |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
          in
          List.iter modules ~f:(fun m -> if fine_module m then on_module extra_paths m)
*)
      | Library { Library.modules; requires } ->
        let extra_paths =
          requires
          |> List.filter_map ~f:(fun uid -> get_library uid)
          |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
        in
        List.iter modules ~f:(fun m ->
          let info =
            ref
              { name = m.name
              ; cycl_compl_data = None
              ; cogn_compl_data = None
              ; holsted_for_funcs = None
              ; loc_metric = None
              }
          in
          if fine_module m then on_module extra_paths m info))
  in
  loop_database ()
;;
