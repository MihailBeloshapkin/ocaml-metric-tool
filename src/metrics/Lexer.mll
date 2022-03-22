{
  let num_lines = ref 0
  let num_chars = ref 0
  let num_empty = ref 0
  let commets_data : string list ref = ref []
}

let e_line = ['\n'] ['{'' ''}' '(' ')' '[' ']' ' ' '\t']* ['\n']  

rule scan = parse
  | "\n"        { incr num_lines; scan lexbuf }
  | "(*"        { comments 0 "" lexbuf }
  | e_line      { incr num_empty; empty 0 lexbuf }
  | _           { incr num_chars; scan lexbuf }
  | eof         { () }
and comments level acc = parse
  | "(*"        { comments (level + 1) (String.concat "" [acc; "(*"]) lexbuf }
  | "*)"        { if level = 0 then (commets_data := acc::(!commets_data); print_string acc;  scan lexbuf)
                  else
                    comments (level - 1) (String.concat "" [acc; "*)"]) lexbuf }
  | _ as c      { comments level (String.concat "" [acc; String.make 1 c]) lexbuf }
and empty acc = parse
  | ['{'' ''}' '(' ')' '[' ']' ' ' '\t']  { empty acc lexbuf }
  | "\n"                                   { empty (acc + 1) lexbuf }
  | _                                      { (num_empty := !num_empty + acc); scan lexbuf }

{
  let process source =
    let lexbuf = Lexing.from_string source in
    scan lexbuf;
    (!num_empty, !num_lines, !num_chars, !commets_data)
}