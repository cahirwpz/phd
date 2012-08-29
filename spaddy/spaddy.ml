let main () =
  if Array.length Sys.argv > 1
  then
   for i = 1 to Array.length Sys.argv - 1 do
     let filename = Sys.argv.(i) in
     let file = open_in filename in
     Printf.printf "**** %s ****\n" filename;
     Token.print (Lexer.lex (Stream.of_channel file));
     close_in file
   done
  else Token.print (Lexer.lex (Stream.of_channel stdin))

let _ = Printexc.print main ()
