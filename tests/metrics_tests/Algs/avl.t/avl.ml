open Base
open Caml.Format

type avl_tree =
  | Node of int * int * avl_tree * avl_tree
  | Empty

let recalculate_height tree =
  let rec sub t =
    match t with
    | Node (value, _, Empty, Empty) -> Node (value, 1, Empty, Empty), 1
    | Node (value, _, left, right) ->
      let left_node_height = sub left in
      let right_node_height = sub right in
      let new_height = Int.max (left_node_height |> snd) (right_node_height |> snd) + 1 in
      ( Node (value, new_height, left_node_height |> fst, right_node_height |> fst)
      , new_height )
    | Empty -> Empty, 0
  in
  tree |> sub |> fst
;;

exception NodeNotFound

let delete_max_in_subtree current_node =
  let rec find_max current =
    match current with
    | Node (value, _, _, Empty) -> value
    | Node (_, _, _, right) -> find_max right
    | Empty -> failwith "Empty"
  in
  let rec delete current =
    match current with
    | Node (_, _, left, Empty) -> left
    | Node (value, height, left, right) -> Node (value, height, left, delete right)
    | _ -> failwith "Incorrect sub tree"
  in
  find_max current_node, delete current_node
;;

let left_rotation q =
  match q with
  | Node (qValue, qHeight, a, p) ->
    let new_node =
      match p with
      | Node (pValue, pHeight, b, c) ->
        Node (pValue, pHeight, Node (qValue, qHeight, a, b), c)
      | _ -> failwith "Incorrect node"
    in
    new_node
  | _ -> q
;;

let right_rotation p =
  match p with
  | Node (pValue, pHeight, q, c) ->
    let new_node =
      match q with
      | Node (qValue, qHeight, a, b) ->
        Node (qValue, qHeight, a, Node (pValue, pHeight, b, c))
      | _ -> failwith "Incorrect node"
    in
    new_node
  | _ -> p
;;

let height = function
  | Node (_, h, _, _) -> h
  | _ -> 0
;;

let balance_factor = function
  | Node (_, _, left, right) -> height right - height left
  | Empty -> 0
;;

let get_list t =
  let rec lowest_unbalanced tr l =
    match tr with
    | Node (value, _, left, right) ->
      if tr |> balance_factor = 2
      then lowest_unbalanced right (value :: l)
      else if tr |> balance_factor = -2
      then lowest_unbalanced left (value :: l)
      else lowest_unbalanced left [] @ lowest_unbalanced right [] @ l
    | Empty -> l
  in
  [] |> lowest_unbalanced t
;;

let balance_vertex (value, height, left, right) =
  let t_balance_factor = balance_factor (Node (value, height, left, right)) in
  match t_balance_factor, balance_factor left, balance_factor right with
  | 2, _, r when r < 0 ->
    Node (value, height, left, right_rotation right) |> left_rotation
  | 2, _, r when r > -1 -> Node (value, height, left, right) |> left_rotation
  | -2, l, _ when l > 0 ->
    Node (value, height, left_rotation left, right) |> right_rotation
  | -2, l, _ when l < 1 -> Node (value, height, left, right) |> right_rotation
  | _ -> failwith "Error"
;;

let balance tree =
  let lowest_unbalanced = tree |> get_list |> List.hd in
  match lowest_unbalanced with
  | None -> tree
  | Some lowest_unbalanced_vertex ->
    let rec sub_balance t =
      match t with
      | Node (value, height, left, right) when lowest_unbalanced_vertex < value ->
        Node (value, height, sub_balance left, right)
      | Node (value, height, left, right) when lowest_unbalanced_vertex > value ->
        Node (value, height, left, sub_balance right)
      | Node (value, height, left, right) when lowest_unbalanced_vertex = value ->
        balance_vertex (value, height, left, right)
      | _ -> Empty
    in
    tree |> sub_balance |> recalculate_height
;;

let add_node new_node tree =
  let rec sub node =
    match node with
    | Empty -> Node (new_node, 0, Empty, Empty)
    | Node (current, height, left, right) ->
      if new_node > current
      then Node (current, height, left, sub right)
      else Node (current, height, sub left, right)
  in
  tree |> sub |> recalculate_height |> balance
;;

let delete_node node_to_delete tree =
  let rec sub_delete current_node =
    match current_node with
    | Node (value, height, l, r) when value <> node_to_delete ->
      if node_to_delete > value
      then Node (value, height, l, sub_delete r)
      else Node (value, height, sub_delete l, r)
    | Node (_, height, left, right) ->
      let result =
        match left with
        | Empty -> right
        | _ ->
          let max_in_left_sub_tree, new_left = delete_max_in_subtree left in
          Node (max_in_left_sub_tree, height, new_left, right)
      in
      result
    | _ -> Empty
  in
  tree |> sub_delete |> recalculate_height |> balance
;;