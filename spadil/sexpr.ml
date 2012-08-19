open Format;;

type sexpr =
  | Group of sexpr list 
  | Label of string
  | Number of float
  | Quote of sexpr
  | String of string
  | Symbol of string 
  | TreeDecl of int * sexpr
  | TreeRef of int

let rec split n lst =
  split_rec n lst []
and split_rec n lst acc =
  if n > 0
  then split_rec (n - 1) (List.tl lst) (List.hd lst::acc)
  else (List.rev acc, lst)

let rec print e =
  print_rec e;
  printf "@."
and print_list_sep sep = function
  | head::(_::_ as tail) ->
      print_rec head;
      printf sep; 
      print_list_sep sep tail
  | head::_ ->
      print_rec head;
  | [] -> ()
and print_list lst = print_list_sep " " lst
and print_form_sep n sep lst =
  let (fst, snd) = split n lst in
  printf "@[<v>";
  print_list fst;
  printf "@,";
  print_list_sep sep snd;
  printf "@]"
and print_form n lst = print_form_sep n " " lst
and print_group = function
  | Symbol "defun"::_ as g -> print_form 3 g
  | Symbol "lambda"::_
  | Symbol "prog"::_ as g -> print_form 2 g
  | Symbol "loop"::_
  | Symbol "progn"::_
  | Symbol "return"::_
  | Symbol "or"::_
  | Symbol "and"::_
  | Symbol "cond"::_ as g -> print_form_sep 1 "@," g
  | _ as e -> print_list e
and print_rec = function
  | Group l -> printf "@[<v 1>("; print_group l; printf ")@]"
  | Label s -> printf "#:%s" s
  | Number n -> print_float n
  | Quote e -> print_char '\''; print_rec e
  | String s -> printf "\"%s\"" s
  | Symbol s -> printf "|%s|" s
  | TreeDecl (n, expr) -> printf "#%d=" n; print_rec expr
  | TreeRef n -> printf "#%d#" n

let slots = Array.create 20 (Group [])

let rec reduce = function
  | TreeDecl (n, expr) ->
      let reduced = reduce expr in
      slots.(n) <- reduced;
      reduced
  | TreeRef n ->
      slots.(n)
  | Group g ->
      Group (List.map reduce g)
  | Quote q ->
      Quote (reduce q)
  | _ as expr -> expr
