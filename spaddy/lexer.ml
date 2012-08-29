open Token

let rec lex = parser
  (* Indentation: [\s\t]+ *)
  | [< ' (' '); stream >] ->
      lex_indent 1 stream
  | [< ' ('\t'); stream >] ->
      lex_indent 8 stream
  | [< ' ('\n'); stream=lex >] ->
      Printf.printf "%d\n" (Stream.count stream);
      [< 'Eol; stream >]

  (* Escaping delimeter. *)
  | [< ' ('_'); stream >] ->
      begin parser
        | [< ' ('\n'); stream=lex >] ->
            [< 'Lc; stream >]
        | [< stream=lex >] ->
            [< stream >]
      end stream
  
  (* Strings (the contents is being unescaped). *)
  | [< ' ('"'); stream >] ->
      let buffer = Buffer.create 1 in
      lex_string buffer stream

  (* number: [0-9.]+ *)
  | [< ' ('0' .. '9' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_number buffer stream

  (* One character tokens: '()[]$@*,;^' *)
  | [< ' ('('); stream=lex >] -> [< 'LParen; stream >]
  | [< ' (')'); stream=lex >] -> [< 'RParen; stream >]
  | [< ' ('['); stream=lex >] -> [< 'LBracket; stream >]
  | [< ' (']'); stream=lex >] -> [< 'RBracket; stream >]
  | [< ' ('$'); stream=lex >] -> [< 'UsesType; stream >]
  | [< ' ('@'); stream=lex >] -> [< 'ReturnsType; stream >]
  | [< ' ('*'); stream=lex >] -> [< 'Times; stream >]
  | [< ' (','); stream=lex >] -> [< 'Comma; stream >]
  | [< ' (';'); stream=lex >] -> [< 'Semicolon; stream >]
  | [< ' ('^'); stream=lex >] -> [< 'Pow; stream >]
  | [< ' ('#'); stream=lex >] -> [< 'Length; stream >]
  | [< ' ('%'); stream=lex >] -> [< 'Self; stream >]
  | [< ' ('\''); stream=lex >] -> [< 'Quote; stream >]

  (* Unambiguous two character tokens: '\/' *)
  | [< ' ('\\'); ' ('/'); stream=lex >] -> [< 'Or; stream >]

  (* Ambiguous tokens that begin with colon character - ':', ':=', '::'. *)
  | [< ' (':'); stream >] ->
      begin parser
        | [< ' (':'); stream=lex >] ->
            [< 'ToType; stream >]
        | [< ' ('='); stream=lex >] ->
            [< 'Assign; stream >]
        | [< stream=lex >] ->
            [< 'OfType; stream >]
      end stream

  (* Ambiguous tokens that begin with equals character - '=', '==', '==>'. *)
  | [< ' ('='); stream >] ->
      begin parser
        | [< ' ('='); stream >] ->
            begin parser
              | [< ' ('>'); stream=lex >] ->
                  [< 'Macro; stream >]
              | [< stream=lex >] ->
                  [< 'Bind; stream >]
            end stream
        | [< stream=lex >] ->
            [< 'Eq; stream >]
      end stream

  (* Ambiguous tokens that begin with plus character - '+', '++', '+->'. *)
  | [< ' ('+'); stream >] ->
      begin parser
        | [< ' ('+'); stream >] ->
            lex_comment (Buffer.create 1) stream
        | [< ' ('-'); ' ('>'); stream=lex >] ->
            [< 'Lambda; stream >]
        | [< stream=lex >] ->
            [< 'Plus; stream >]
      end stream

  (* Ambiguous tokens that begin with minus character - '-', '->', '--'. *)
  | [< ' ('-'); stream >] ->
      begin parser
        | [< ' ('>'); stream=lex >] ->
            [< 'Arrow; stream >]
        | [< ' ('-'); stream >] ->
            lex_comment (Buffer.create 1) stream
        | [< stream=lex >] ->
            [< 'Minus; stream >]
      end stream

  (* Ambiguous tokens that begin with dot character - '.', '..'. *)
  | [< ' ('.'); stream >] ->
      begin parser
        | [< ' ('.'); stream=lex >] ->
            [< 'Ellipsis; stream >]
        | [< stream=lex >] ->
            [< 'Dot; stream >]
      end stream

  (* Ambiguous tokens that begin with slash character - '/', '/\'. *)
  | [< ' ('/'); stream >] ->
      begin parser
        | [< ' ('\\'); stream=lex >] ->
            [< 'And; stream >]
        | [< stream=lex >] ->
            [< 'By; stream >]
      end stream

  (* Ambiguous tokens that begin with less-than character - '<', '<='. *)
  | [< ' ('<'); stream >] ->
      begin parser
        | [< ' ('='); stream=lex >] ->
            [< 'Le; stream >]
        | [< stream=lex >] ->
            [< 'Lt; stream >]
      end stream

  (* Ambiguous tokens that begin with greater-than character - '>', '>='. *)
  | [< ' ('>'); stream >] ->
      begin parser
        | [< ' ('='); stream=lex >] ->
            [< 'Ge; stream >]
        | [< stream=lex >] ->
            [< 'Gt; stream >]
      end stream

  (* Ambiguous tokens that begin with greater-than character - '~', '~='. *)
  | [< ' ('~'); stream >] ->
      begin parser
        | [< ' ('='); stream=lex >] ->
            [< 'Ne; stream >]
        | [< stream=lex >] ->
            [< 'Not; stream >]
      end stream

  (* Finally identifiers: functions, variables, types, etc. *)
  | [< ' ('A'..'Z' | 'a'..'z' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_id buffer stream

  (* end of stream. *)
  | [< >] -> [< >]

and lex_comment buffer = parser
  | [< ' ('\n'); stream=lex >] ->
      [< 'Comment (Buffer.contents buffer); 'Eol; stream >]
  | [< 'c; stream >] ->
      Buffer.add_char buffer c;
      lex_comment buffer stream

and lex_indent spaces = parser
  | [< ' (' '); stream >] ->
      lex_indent (spaces + 1) stream
  | [< ' ('\t'); stream >] ->
      lex_indent (spaces + 8) stream
  | [< stream=lex >] ->
      [< 'Indent spaces; stream >]

and lex_string buffer = parser
  | [< ' ('\\'); 'c; stream >] ->
      Buffer.add_char buffer '\\';
      Buffer.add_char buffer c;
      lex_string buffer stream
  | [< ' ('"'); stream=lex >] ->
      let s = Buffer.contents buffer in
      [< 'String (Scanf.unescaped s); stream >]
  | [< 'c; stream >] ->
      Buffer.add_char buffer c;
      lex_string buffer stream

and lex_id buffer = parser
  | [< ' ('A'..'Z' | 'a'..'z' | '0'..'9' | '?' | '!' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_id buffer stream
  | [< stream=lex >] ->
      let name = Buffer.contents buffer in begin
        match name with
        | "add"            -> [< 'Add; stream >]
        | "and"            -> [< 'And; stream >]
        | "break"          -> [< 'Break; stream >]
        | "by"             -> [< 'Step; stream >]
        | "case"           -> [< 'HasType; stream >]
        | "else"           -> [< 'Else; stream >]
        | "error"          -> [< 'Error; stream >]
        | "exquo"          -> [< 'Exquo; stream >]
        | "for"            -> [< 'For; stream >]
        | "has"            -> [< 'Has; stream >]
        | "if"             -> [< 'If; stream >]
        | "import"         -> [< 'Import; stream >]
        | "in"             -> [< 'In; stream >]
        | "iterate"        -> [< 'Continue; stream >]
        | "not"            -> [< 'Not; stream >]
        | "or"             -> [< 'Or; stream >]
        | "pretend"        -> [< 'Pretend; stream >]
        | "quo"            -> [< 'Quo; stream >]
        | "rem"            -> [< 'Rem; stream >]
        | "return"         -> [< 'Return; stream >]
        | "then"           -> [< 'Then; stream >]
        | "when"           -> [< 'When; stream >]
        | "where"          -> [< 'Where; stream >]
        | "with"           -> [< 'With; stream >]
        | _ ->
            begin
              match name.[0] with
              | 'a'..'z' ->
                  [< 'Name name; stream >]
              | _ ->
                  [< 'TypeName name; stream >]
            end
      end

and lex_number buffer = parser
  | [< ' ('0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_number buffer stream
  | [< ' ('.'); stream >] ->
      Buffer.add_char buffer '.';
      lex_float buffer stream
  | [< stream=lex >] ->
      [< 'Int (int_of_string (Buffer.contents buffer)); stream >]

and lex_float buffer = parser
  | [< ' ('0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_float buffer stream
  | [< stream=lex >] ->
      [< 'Float (float_of_string (Buffer.contents buffer)); stream >]
