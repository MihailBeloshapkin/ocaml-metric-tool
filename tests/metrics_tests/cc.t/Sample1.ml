let f x = if x > 0 then x else 0;;

let g x y =
  match x with
  | 3 -> if y > 0 then y else x
  | _ -> x
;;

let z x y =
  if y > 0 then
    if x < 9 then 0 else x
  else 
    if x > 10 then 5 else 0  
;;

let h x y =
  if x > 0 then
    match y with
    | 0 -> 0
    | 1 -> 8
    | _ -> y
  else 
    100
;;

let t x y =
  let a = if x > y then x else 0 in
  a
;;

let analyze_lident (txt : Longident.t) op_type =
  (*TODO: Solve problem with long identifiers*)
  let rec loop_ident tx acc =
    match txt with
    | Lident(info) -> add_helsted_info info op_type acc     
    | Ldot(t, info) -> 
      add_helsted_info info op_type acc
    | Lapply(t1, t2) -> 
      let fst_data = loop_ident t1 acc in
      let snd_data = loop_ident t2 acc in
      { operators = (List.concat [fst_data.operators; snd_data.operators]); operands = (List.concat [fst_data.operands; snd_data.operands]) }
  in
  loop_ident txt { operators = []; operands = [] }
;;
