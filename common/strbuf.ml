(* String buffer class - a simple wrapper for stdlib's Buffer. *)
class strbuf =
  object (self)
    val buffer = Buffer.create 1
    method putc c =
      Buffer.add_char buffer c
    method puts s = 
      Buffer.add_string buffer s
    method gets =
      Scanf.unescaped (Buffer.contents buffer)
  end
