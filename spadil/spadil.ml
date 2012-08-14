let main () =
  let input =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel input in
  let tree = Parser.program Lexer.token lexbuf in
  Ast.print tree

let _ = Printexc.print main ()
