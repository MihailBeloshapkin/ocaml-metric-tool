open Base
open Caml.Format

type avl_tree =
  | Node of int * int * avl_tree * avl_tree
  | Empty
;;

(* Add node to a tree *)
let add_node new_node tree =
  let rec sub node =
    match node with
    | Empty -> Node (new_node, 0, Empty, Empty)
    | Node (current, height, left, right) ->
      if new_node > current then Node (current, height, left, sub right)
      else Node (current, height, sub left, right)
  in
  sub tree
;;

let recalculate_height tree =
  let rec sub t = 
    match t with
    | Node(value, height, Empty, Empty) -> (Node(value, 1, Empty, Empty), 1)
    | Node(value, height, left, right) -> 
      let nodeHeightL = sub left in
      let nodeHeightR = sub right in
      let newHeight = (Int.max (nodeHeightL |> snd) (nodeHeightR |> snd)) + 1 in
      (Node(value, newHeight, nodeHeightL |> fst, nodeHeightR |> fst), newHeight)
    | Empty -> (Empty, 0)
  in
  tree |> sub |> fst
;;

exception NodeNotFound

let delete_max_in_subtree current_node =
  let rec find_max current =  
    match current with
    | Node(value,  _, _, Empty) -> value
    | Node(_, _, left, right) -> find_max right
    | Empty -> failwith "Empty"
  in
  let rec delete current =
    match current with
    | Node(value, height, left, Empty) -> left
    | Node(value, height, left, right) -> Node(value, height, left, delete right)
    | _ -> failwith "Incorrect sub tree"
  in
  (find_max current_node, delete current_node)
;;

let left_rotation q =
  match q with
  | Node(qValue, qHeight, a, p) -> 
    let new_node =
      match p with
      | Node(pValue, pHeight, b, c) -> Node(pValue, pHeight, Node(qValue, qHeight, a, b), c)  
      | _ -> failwith "Incorrect node"
    in new_node
  | _ -> q
;;

let right_rotation p =
  match p with
  | Node(pValue, pHeight, q, c) -> 
    let new_node = 
      match q with
      | Node(qValue, qHeight, a, b) -> Node(qValue, qHeight, a, Node(pValue, pHeight, b, c))
      | _ -> failwith "Incorrect node"     
    in new_node
  | _ -> p
;;

let delete_node node_to_delete tree = 
  let rec sub_delete current_node = 
    match current_node with
    | Node (value, height, l, r) when value <> node_to_delete ->
      if node_to_delete > value then Node(value, height, l, sub_delete r) else Node(value, height, sub_delete l, r)
    | Node (value, height, left, right) ->
      let result =
        match left with
        | Empty -> right
        | _ -> 
          let (max_in_left_sub_tree, new_left) = delete_max_in_subtree left in
          Node(max_in_left_sub_tree, height, new_left, right)
      in result
    | _ -> Empty 
    in
  sub_delete tree
;;

(* Display *)
let rec output tree =
  match tree with
  | Empty -> ()
  | Node (value, height, left, right) -> 
    Caml.Format.printf "(%i, %i)" value height;
    output left;
    output right;
;;

let () =
  let tree = Empty |> add_node 5 |> add_node 7 |> add_node 9 |> add_node 1 in
  output tree;
  printf "After deletion: \n";
  let new_tree  = tree |> delete_node 9 in
  output new_tree;
  ()
;;