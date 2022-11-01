  $ dune build
  $ mylinter -m loc avl.ml
  
  Module: avl
  
  
    LOC:
      Lines: 74
      Logical lines: 58
      Comments: 2
  $ mylinter -m cc avl.ml
  
  Module: avl
  
  
    func: add_node
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 3
  
  
    func: delete_max_in_subtree
    Cyclomatic complexity:
      without CFG: 3
      with CFG: 5
  
  
    func: delete_node
    Cyclomatic complexity:
      without CFG: 4
      with CFG: 5
  
  
    func: output
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 2
  
  $ mylinter -m cg avl.ml
  
  Module: avl
  
  
    func: add_node
    Cognitive complexity: 4
  
    func: delete_max_in_subtree
    Cognitive complexity: 4
  
    func: delete_node
    Cognitive complexity: 6
  
    func: output
    Cognitive complexity: 1
  $ mylinter -m halstead avl.ml
  
  Module: avl
  
  func: 0
    Operators: let match if-then > sub sub sub 
    Operands: current new_node right left tree 
    Unique Operators: sub > if-then match let 
    Unique Operands: tree left right new_node current 
    Volume: 12.000000 Theoretical volume: 16.094379 
  
  func: 1
    Operators: let match find_max failwith let match delete failwith find_max delete 
    Operands: right Empty right Incorrect sub tree current_node current_node 
    Unique Operators: delete failwith find_max match let 
    Unique Operands: current_node Incorrect sub tree Empty right 
    Volume: 16.000000 Theoretical volume: 13.592367 
  
  func: 2
    Operators: let match <> if-then > sub_delete sub_delete let match let delete_max_in_subtree sub_delete 
    Operands: node_to_delete value value node_to_delete r l left tree 
    Unique Operators: delete_max_in_subtree sub_delete > if-then <> match let 
    Unique Operands: tree left l r value node_to_delete 
    Volume: 20.000000 Theoretical volume: 24.371928 
  
  func: 3
    Operators: match printf output output 
    Operands: value %d,  left right 
    Unique Operators: output printf match 
    Unique Operands: right left %d,  value 
    Volume: 8.000000 Theoretical volume: 8.841014 
  
