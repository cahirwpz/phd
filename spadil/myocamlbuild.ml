open Ocamlbuild_plugin;;

flag ["link"; "ocaml"; "use_gc"] &
S[A"-cclib"; A"-L/opt/local/lib"; A"-cclib"; A"-lgc"];;
