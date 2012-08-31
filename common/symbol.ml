(* generate new symbols on demand *)

class gen fmt =
  object
    val fmt : (int -> string, unit, string) format = fmt
    val mutable counter = 0
    method get =
      let symbol = Printf.sprintf fmt counter
      in counter <- counter + 1; symbol
    method reset =
      counter <- 0
  end
