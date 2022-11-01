open Base
open Caml.Format

type avl_tree =
  | Node of int * avl_tree * avl_tree
  | Empty
;;

(* Add node to a tree *)
let add_node new_node tree =
  let rec sub node =
    match node with
    | Empty -> Node (new_node, Empty, Empty)
    | Node (current, left, right) ->
      if new_node > current then Node (current, left, sub right)
      else Node (current, sub left, right)
  in
  sub tree
;;

exception NodeNotFound

let delete_max_in_subtree current_node =
  let rec find_max current =
    match current with
    | Node(value,  _, Empty) -> value
    | Node(_, left, right) -> find_max right
    | Empty -> failwith "Empty"
  in
  let rec delete current =
    match current with
    | Node(value, left, Empty) -> left
    | Node(value, left, right) -> Node(value, left, delete right)
    | _ -> failwith "Incorrect sub tree"
  in  
  (find_max current_node, delete current_node)
;;

let delete_node node_to_delete tree = 
  let rec sub_delete current_node = 
    match current_node with
    | Node (value, l, r) when value <> node_to_delete ->
      if node_to_delete > value then Node(value, l, sub_delete r) else Node(value, sub_delete l, r)
    | Node (value, left, right) ->
      let result =
        match left with
        | Empty -> right
        | _ -> 
          let (max_in_left_sub_tree, new_left) = delete_max_in_subtree left in
          Node(max_in_left_sub_tree, new_left, right)
      in result
    | _ -> Empty 
    in
  sub_delete tree
;;

(* Display *)
let rec output tree =
  match tree with
  | Empty -> ()
  | Node (value, left, right) -> 
    Caml.Format.printf "%d, " value;
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