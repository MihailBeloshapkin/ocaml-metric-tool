open Caml
open Base
open Zanuda_core
open Utils
open Addition

let untyped_linters =
  let open UntypedLints in
  let open Metrics in
  [ (module GuardInsteadOfIf : LINT.UNTYPED)
  ; (module Casing : LINT.UNTYPED)
  ; (module ParsetreeHasDocs : LINT.UNTYPED)
  ; (module ToplevelEval : LINT.UNTYPED)
  ; (module VarShouldNotBeUsed : LINT.UNTYPED)
  ]
;;

let typed_linters =
  let open TypedLints in
  [ (* * *********************** *)
    (module Failwith : LINT.TYPED)
  ; (module Hashtables : LINT.TYPED)
  ; (module ListLength : LINT.TYPED)
  ; (module ProposeFunction : LINT.TYPED)
  ; (module ExcTryWithWildcard : LINT.TYPED)
  ; (module Record1 : LINT.TYPED)
  ; (module Ignore : LINT.TYPED)
  ; (module ListFusion : LINT.TYPED)
  ; (module IfBool : LINT.TYPED)
  ; (module Equality : LINT.TYPED)
  ; (module StringConcat : LINT.TYPED)
  ; (module MonadLaws : LINT.TYPED) (* * *********************** *)
  ]
;;

(* TODO: Functions below are a little bit copy-pasty. Rework them *)
let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let untyped_on_structure info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.structure o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let untyped_on_signature info =
  build_iterator
    ~f:(fun o -> o.Ast_iterator.signature o)
    ~compose:(fun (module L : LINT.UNTYPED) -> L.run info)
    ~init:Ast_iterator.default_iterator
    untyped_linters
;;

let typed_on_structure info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let typed_on_signature info =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun (module L : LINT.TYPED) -> L.run info)
    ~init:Tast_iterator.default_iterator
    typed_linters
;;

let with_info filename f =
  Compile_common.with_info
    ~native:false
    ~source_file:filename
    ~tool_name:"asdf" (* TODO: pass right tool name *)
    ~output_prefix:"asdf"
    ~dump_ext:"asdf"
    f
;;

let process_cmt_typedtree filename typedtree =
  if Config.verbose () then printfn "Analyzing cmt: %s" filename;
  (* Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree; *)
  with_info filename (fun info -> typed_on_structure info typedtree)
;;

let process_cmti_typedtree filename typedtree =
  (* if Config.Options.verbose ()
  then (
    let () = printfn "Analyzing cmti: %s" filename in
    printfn "%a" Printtyped.interface typedtree); *)
  (* Format.printf "Typedtree MLI:\n%a\n%!" Printtyped.interface typedtree; *)
  with_info filename (fun info -> typed_on_signature info typedtree)
;;

let process_metrics ~path_to_save ~parsetree ~filename ~metric ~info =
  let open Metrics in
  let open Parsetree in
  let open GetStatistics in
  (*
  let it = run Ast_iterator.default_iterator in
  it.structure it parsetree;
  *)
  match metric with
  | "loc" -> LOC.run filename info
  | "halstead" -> Holsted.run parsetree info
  | "cc" ->
    let () =
      match path_to_save with
      | Some p -> CCComplexity.run ~path_to_save:p parsetree info
      | None -> CCComplexity.run parsetree info
    in
    ()
  | "cg" -> CognitiveComplexity.run parsetree info
  | "all" ->
    LOC.run filename info;
    Holsted.run parsetree info;
    CCComplexity.run parsetree info;
    CognitiveComplexity.run parsetree info
  | _ -> ()
;;

let process ~path_to_save_cfg ~metric linfo filename =
  try
    Clflags.error_style := Some Misc.Error_style.Contextual;
    Clflags.include_dirs := Config.includes () @ Clflags.include_dirs.contents;
    let with_info f =
      Compile_common.with_info
        ~native:false
        ~source_file:filename
        ~tool_name:"asdf"
        ~output_prefix:"asdf"
        ~dump_ext:"asdf"
        f
    in
    let () =
      let process_structure info =
        let parsetree = Compile_common.parse_impl info in
        (*let typedtree, _ = Compile_common.typecheck_impl info parsetree in*)
        (*untyped_on_structure info parsetree; *)
        process_metrics
          ~path_to_save:path_to_save_cfg
          ~parsetree
          ~filename
          ~metric
          ~info:linfo
      in
      with_info (fun info ->
        if String.is_suffix info.source_file ~suffix:".ml"
        then process_structure info
        else (
          let () =
            Caml.Format.eprintf
              "Don't know to do with file '%s'\n%s %d\n%!"
              info.source_file
              Caml.__FILE__
              Caml.__LINE__
          in
          Caml.exit 1))
    in
    ()
  with
  | Sys_error _ -> 
    printfn "Something went wrong. Make sure that directory path is correct and project was build with dune"
;;

let () =
  let open Config in
  let open StatisticsCollector in
  let open StatisticsCollector.ModuleInfo in
  let open ProcessXml in
  parse_args ();
  let () =
    match mode () with
    | Config.Unspecified -> ()
    | File (filename, metric, path_to_save_cfg) ->
      let info =
        ref
          { name = Filename.chop_suffix filename ".ml"
          ; cycl_compl_data = None
          ; cogn_compl_data = None
          ; holsted_for_funcs = None
          ; loc_metric = None
          }
      in
      process ~path_to_save_cfg ~metric info filename;
      add_module_info !info
    | Dir (source, metric, path_to_save_cfg) ->
      ProcessDune.analyze_directory source (process ~path_to_save_cfg ~metric)
    | _ -> ()
  in
  write_data_to_xml !StatisticsCollector.common_data (open_out "output.xml");
  (* report_all (); *)
  ()
;;
