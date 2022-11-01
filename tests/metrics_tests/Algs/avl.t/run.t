  $ dune build
  $ mylinter -m all avl.ml
  
  Module: avl
  
    LOC:
      Lines: 152
      Logical lines: 128
      Comments: 0
  
    func: recalculate_height
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 3
  
  
    func: delete_max_in_subtree
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 5
  
  
    func: left_rotation
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 3
  
  
    func: right_rotation
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 3
  
  
    func: height
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 2
  
  
    func: balance_factor
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 2
  
  
    func: get_list
    Cyclomatic complexity:
      without CFG: 4
      with CFG: 4
  
  
    func: balance_vertex
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 5
  
  
    func: balance
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 5
  
  
    func: add_node
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 3
  
  
    func: delete_node
    Cyclomatic complexity:
      without CFG: 4
      with CFG: 5
  
  
    func: recalculate_height
    Cognitive complexity: 2
  
    func: delete_max_in_subtree
    Cognitive complexity: 4
  
    func: left_rotation
    Cognitive complexity: 3
  
    func: right_rotation
    Cognitive complexity: 3
  
    func: height
    Cognitive complexity: 1
  
    func: balance_factor
    Cognitive complexity: 1
  
    func: get_list
    Cognitive complexity: 7
  
    func: balance_vertex
    Cognitive complexity: 1
  
    func: balance
    Cognitive complexity: 4
  
    func: add_node
    Cognitive complexity: 4
  
    func: delete_node
    Cognitive complexity: 6
  func: 0
    Operators: let match let sub let sub let + max |> |> |> |> |> |> 
    Operands: 1 1 left right snd left_node_height snd right_node_height 1 fst left_node_height fst right_node_height 0 fst sub tree 
    Unique Operators: |> max + sub match let 
    Unique Operands: tree sub 0 fst right_node_height left_node_height snd right left 1 
    Volume: 32.000000 Theoretical volume: 33.776408 
  
  func: 1
    Operators: let match find_max failwith let match delete failwith find_max delete 
    Operands: right Empty right Incorrect sub tree current_node current_node 
    Unique Operators: delete failwith find_max match let 
    Unique Operands: current_node Incorrect sub tree Empty right 
    Volume: 16.000000 Theoretical volume: 13.592367 
  
  func: 2
    Operators: match let match failwith 
    Operands: Incorrect node 
    Unique Operators: failwith let match 
    Unique Operands: Incorrect node 
    Volume: 5.000000 Theoretical volume: 3.295837 
  
  func: 3
    Operators: match let match failwith 
    Operands: Incorrect node 
    Unique Operators: failwith let match 
    Unique Operands: Incorrect node 
    Volume: 5.000000 Theoretical volume: 3.295837 
  
  func: 4
    Operators: function 
    Operands: 0 
    Unique Operators: function 
    Unique Operands: 0 
    Volume: 2.000000 Theoretical volume: 0.000000 
  
  func: 5
    Operators: function - height height 
    Operands: right left 0 
    Unique Operators: height - function 
    Unique Operands: 0 left right 
    Volume: 7.000000 Theoretical volume: 6.591674 
  
  func: 6
    Operators: let match if-then = |> lowest_unbalanced if-then = |> lowest_unbalanced @ lowest_unbalanced @ lowest_unbalanced |> lowest_unbalanced 
    Operands: balance_factor tr 2 right balance_factor tr -2 left left l right t 
    Unique Operators: @ lowest_unbalanced |> = if-then match let 
    Unique Operands: t l left -2 right 2 tr balance_factor 
    Volume: 28.000000 Theoretical volume: 30.256903 
  
  func: 7
    Operators: let balance_factor match balance_factor balance_factor < |> right_rotation > |> > |> left_rotation < |> failwith 
    Operands: left right r 0 left_rotation right r -1 left_rotation l 0 right_rotation left l 1 right_rotation Error 
    Unique Operators: failwith left_rotation > right_rotation |> < match balance_factor let 
    Unique Operands: Error 1 right_rotation l -1 left_rotation 0 r right left 
    Volume: 33.000000 Theoretical volume: 42.800872 
  
  func: 8
    Operators: let |> |> match let match < sub_balance > sub_balance = balance_vertex |> |> 
    Operands: hd get_list tree value lowest_unbalanced_vertex left value lowest_unbalanced_vertex right value lowest_unbalanced_vertex recalculate_height sub_balance tree 
    Unique Operators: balance_vertex = > sub_balance < match |> let 
    Unique Operands: sub_balance recalculate_height right left lowest_unbalanced_vertex value tree get_list hd 
    Volume: 28.000000 Theoretical volume: 36.410554 
  
  func: 9
    Operators: let match if-then > sub sub |> |> |> 
    Operands: 0 current new_node right left balance recalculate_height sub tree 
    Unique Operators: |> sub > if-then match let 
    Unique Operands: tree sub recalculate_height balance left right new_node current 0 
    Volume: 18.000000 Theoretical volume: 30.525578 
  
  func: 10
    Operators: let match <> if-then > sub_delete sub_delete let match let delete_max_in_subtree |> |> |> 
    Operands: node_to_delete value value node_to_delete r l left balance recalculate_height sub_delete tree 
    Unique Operators: |> delete_max_in_subtree sub_delete > if-then <> match let 
    Unique Operands: tree sub_delete recalculate_height balance left l r value node_to_delete 
    Volume: 25.000000 Theoretical volume: 36.410554 
  
