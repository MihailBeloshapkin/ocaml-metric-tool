  $ dune build
  $ mylinter -m cc -dir .   
  
  Module: Main
  
  
    func: f
    Cyclomatic complexity:
      without CFG: 1
      with CFG: 1
  
  
    func: g
    Cyclomatic complexity:
      without CFG: 2
      with CFG: 2
  
  
  Module: Mod1
  
  
    func: a
    Cyclomatic complexity:
      without CFG: 1
      with CFG: 1
  
  
    func: b
    Cyclomatic complexity:
      without CFG: 1
      with CFG: 1
  
  $ mylinter -m loc -dir .
  
  Module: Main
  
  
    LOC:
      Lines: 8
      Logical lines: 5
      Comments: 0
  
  Module: Mod1
  
  
    LOC:
      Lines: 3
      Logical lines: 1
      Comments: 0
