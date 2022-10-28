open Base
open Xmlm
open StatisticsCollector
open StatisticsCollector.ModuleInfo

let tag n = ("", n), [] 

let tag_with_attrs n a = ("", n), a

let process_cg output_el cg =
  let open StatisticsCollector.CognComplexity in
  let value = string_of_int cg.cogn_complexity in
  let attrs = [(("", "name"), cg.func_name); (("", "value"), value)] in
  output_el (tag_with_attrs "Function" attrs) ""; 
;;

let process_cc output_el cc =
  let open StatisticsCollector.CyclComplexity in
  let without_cfg = string_of_int cc.complexity in
  let with_cfg = string_of_int cc.complexity_with_cfg in
  let attrs = [(("", "name"), cc.func_name); (("", "without cfg"), without_cfg); (("", "with cfg"), with_cfg)] in
  output_el (tag_with_attrs "Function" attrs) "";
;;

let process_loc output_el loc =
  let open StatisticsCollector.Loc in
  let lines = string_of_int loc.lines in
  let lloc = string_of_int loc.lloc in
  let comments = string_of_int loc.comments in
  let attrs = [(("", "Lines"), lines); (("", "Logic Lines"), lloc); (("", "Comments"), comments)] in
  List.iter ~f:(fun at -> output_el (tag_with_attrs "LOC:" [at]) "") attrs;
;;

let process_holsted output_el holsted =
  let open StatisticsCollector.HolsedData in
  let volume = string_of_float holsted.volume in
  let theoretical_volume = string_of_float holsted.theoretical_volume in
  let attrs = [(("", "name"), holsted.func_name); (("", "Volume"), volume); (("", "Theoretical Volume"), theoretical_volume)] in
  output_el (tag_with_attrs "Function" attrs) "";
;;

let out_module out data =
  let cogn_data = data.cogn_compl_data in
  let cycl_data = data.cycl_compl_data in
  let loc_data = data.loc_metric in
  let holsted_data = data.holsted_for_funcs in

  let output_el current_sign d =
    out (`El_start current_sign);
    if not (String.equal d  "") then out (`Data d);
    out `El_end
  in
  let output_module_data () =
    let output_metric name o = 
      out (`El_start (tag_with_attrs "Metric" [(("", "name"), name)]));
      o ();
      out `El_end;  
    in
    output_metric "CognitiveComplexity" (fun _ -> Option.iter ~f:(List.iter ~f:(process_cg output_el)) cogn_data);
    output_metric "CyclomaticComplexity" (fun _ -> Option.iter ~f:(List.iter ~f:(process_cc output_el)) cycl_data);
    output_metric "LOC" (fun _ -> Option.iter ~f:(process_loc output_el) loc_data);
    output_metric "Holsted" (fun _ -> Option.iter ~f:(List.iter ~f:(process_holsted output_el)) holsted_data)
  in
  
  out (`El_start (tag_with_attrs "Module" [(("", "name"), data.name)]));   
  output_module_data ();
  out `El_end;
;;

let setup dst =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  Xmlm.output o
;;

let write_data_to_xml common_data oc =
  let out = setup (`Channel oc) in
  out (`Dtd None);
  out (`El_start (tag "Output"));
  
  common_data
  |> List.iter ~f:(out_module out);

  out (`El_end);
;;