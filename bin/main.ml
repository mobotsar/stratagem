open Stratagem
open Util

let ts = Front.lex "#g.(((#barney_the. #    c.purple_dinosaur c) #t.#f.f) there_we_go_ish)"

let () = print_endline @@ Ls.sol Front.tts ts

let () = print_endline "It's not broken yet!"

let tree = Front.parse_prog @@ ref ts

let rec p (t : Front.term) : unit =
  match t with
  | Name (_, n) -> print_string n
  | App (_, t1, t2) -> print_string "(" ; p t1 ; print_string " " ; p t2 ; print_string ")"
  | Abstr (_, n, t) -> print_string "λ" ; p n ; print_char '.' ; p t

let _ = p tree ; print_endline ""
