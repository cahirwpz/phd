open Lexing

let open_named_lexbuf input fname =
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  lexbuf

let incr_line_num lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1;
                                  pos_bol = pos.pos_cnum; }
