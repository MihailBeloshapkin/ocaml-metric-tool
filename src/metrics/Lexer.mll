{
  let num_lines = ref 1
  let num_chars = ref 0
  let num_empty = ref 0
  let num_comm_lines = ref 0
  let commets_data : string list ref = ref []
}

let e_line = ['\n'] ['{' '}' '[' ']' '(' ')'  ' ' '\t' '|' ';']* ['\n']

rule scan = parse
  | "\n"         { incr num_lines; scan lexbuf }
  | "(*"         { incr num_comm_lines; comments 0 "" lexbuf }
  | e_line       { num_lines := !num_lines + 2; 
                   incr num_empty; 
                   empty 0 lexbuf }
  | _            { incr num_chars; scan lexbuf }
  | eof          { () }
and comments level acc = parse
  | "(*"        { comments (level + 1) (String.concat "" [acc; "(*"]) lexbuf }
  | "*)"        { if level = 0 then (commets_data := acc::(!commets_data); scan lexbuf)
                  else
                    comments (level - 1) (String.concat "" [acc; "*)"]) lexbuf }
  | "\n"        { incr num_lines; incr num_comm_lines; comments level (String.concat "" [acc; "\n"]) lexbuf }
  | _ as c      { comments level (String.concat "" [acc; String.make 1 c]) lexbuf }
  | eof         { () }
and empty acc = parse
  | ['{' '}' '(' ')' '[' ']' ' ' '\t']  { empty acc lexbuf }
  | "\n"                                { incr num_lines; empty (acc + 1) lexbuf }
  | "(*"                                { incr num_comm_lines; (num_empty := !num_empty + acc); comments 0 "" lexbuf }
  | _                                   { (num_empty := !num_empty + acc); scan lexbuf }
  | eof                                 { () }

{
  let process source =
    let lexbuf = Lexing.from_string source in
    scan lexbuf;
    (!num_empty, !num_lines, !num_comm_lines, !commets_data)
}