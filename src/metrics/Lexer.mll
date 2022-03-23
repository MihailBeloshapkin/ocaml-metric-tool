{
  let num_lines = ref 1
  let num_chars = ref 0
  let num_empty = ref 0
  let num_comm_lines = ref 0
  let num_llines = ref 0
  let commets_data : string list ref = ref []
}

let e_line = ['\n'] ['{' '}' '[' ']' '(' ')'  ' ' '\t' '|' ';']* ['\n']

rule scan is_new_line = parse
  | "(*"         { incr num_comm_lines; comments 0 "" is_new_line lexbuf }
  | e_line       { num_lines := !num_lines + 2;
                   if not is_new_line then incr num_llines;
                   incr num_empty;
                   empty 0 lexbuf }
  | "\n"         { incr num_lines; 
                   if not is_new_line then incr num_llines;
                   scan true lexbuf }
  | [' ' '\t']   { incr num_chars; scan is_new_line lexbuf }
  | _            { incr num_chars; scan false lexbuf }
  | eof          { () }
and comments level acc is_nline = parse
  | "(*"        { comments (level + 1) (String.concat "" [acc; "(*"]) is_nline lexbuf }
  | "*)"        { if level = 0 then (commets_data := acc::(!commets_data); scan is_nline lexbuf)
                  else
                    comments (level - 1) (String.concat "" [acc; "*)"]) is_nline lexbuf }
  | "\n"        { incr num_lines; incr num_comm_lines; comments level (String.concat "" [acc; "\n"]) true lexbuf }
  | _ as c      { comments level (String.concat "" [acc; String.make 1 c]) is_nline lexbuf }
  | eof         { () }
and empty acc = parse
  | ['{' '}' '(' ')' '[' ']' ' ' '\t']  { empty acc lexbuf }
  | "\n"                                { incr num_lines; empty (acc + 1) lexbuf }
  | "(*"                                { incr num_comm_lines; (num_empty := !num_empty + acc); comments 0 "" true lexbuf }
  | _                                   { (num_empty := !num_empty + acc); scan false lexbuf }
  | eof                                 { () }

{
  let process source =
    let lexbuf = Lexing.from_string source in
    scan true lexbuf;
    (!num_empty, !num_lines, !num_comm_lines, !commets_data, !num_llines)
}