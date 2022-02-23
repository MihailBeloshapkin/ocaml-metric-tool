open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "ifThenElseCount"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Calculates number of if-then-else expressions
|}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let msg = "Count of if-then constructions"

let report ~filename ~loc =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf pp_print_string msg

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ~code:lint_id
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        pp_print_string
        msg
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run _ fallback =
  { fallback with
    expr =
      (fun self currentExpr ->
        let loc = currentExpr.pexp_loc in
        let filename = loc.Location.loc_start.Lexing.pos_fname in
      
        match currentExpr.pexp_desc with
        | Pexp_let (_, _, _) ->  StatisticsCollector.add LetExp
             
        | Pexp_ifthenelse (cond, th, None) -> CollectedLints.add ~loc (report ~filename ~loc);
                                              self.expr self cond;
                                              self.expr self th;
                                              StatisticsCollector.add IfThenElse
        | Pexp_match (ex, li) -> StatisticsCollector.add Match;
                                 self.expr self ex;
                                 List.iter ~f: (fun c -> self.expr self c.pc_rhs) li

                                 
        
        (*  
        let loc = case.pc_rhs.pexp_loc in
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          CollectedLints.add ~loc (report ~filename ~loc);
          self.expr self cond;
          self.expr self th;
          Option.iter ~f:(self.expr self) el *)
        | _ -> fallback.expr self currentExpr)
  }
;;


