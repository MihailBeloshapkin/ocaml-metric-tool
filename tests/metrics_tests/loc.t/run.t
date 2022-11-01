  $ dune build
  $ mylinter -m loc Sample1.ml
  
  Module: Sample1
  
  
    LOC:
      Lines: 4
      Logical lines: 2
      Comments: 0
  $ mylinter -m loc Sample2.ml
  
  Module: Sample2
  
  
    LOC:
      Lines: 8
      Logical lines: 4
      Comments: 2
  $ mylinter -m loc Sample3.ml
  
  Module: Sample3
  
  
    LOC:
      Lines: 25
      Logical lines: 19
      Comments: 2
