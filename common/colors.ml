type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white]

let color_number = function
  | `black    -> 0
  | `red      -> 1
  | `green    -> 2 
  | `yellow   -> 3
  | `blue     -> 4
  | `magenta  -> 5
  | `cyan     -> 6
  | `white    -> 7

let color_code c =
  Printf.sprintf "\x1b[%d;1m" (30 + color_number c)

let reset_code = "\x1b[0m"
let underline_code = "\x1b[4m"
let inverse_code = "\x1b[7m"

let colorize c s = color_code c ^ s ^ reset_code

let red = colorize `red
let green = colorize `green
let yellow = colorize `yellow
let blue = colorize `blue
let magenta = colorize `magenta 
let cyan = colorize `cyan
let white = colorize `white
let underline s = underline_code ^ s ^ reset_code
let inverse s = inverse_code ^ s ^ reset_code
