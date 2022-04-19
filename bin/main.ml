open Stratagem
open Util

(* let ts = Front.lex "#g.(((#barney_the. #    c.purple_dinosaur c) #t.#f.f) there_we_go_ish)"
   let ts = List.take (List.length ts - 1) ts

   let () = print_endline @@ Ls.sol Front.tts ts

   let () = print_endline "It's not broken yet!"

   let tree = Front.parse_prog ts *)

let term_str = lines_of Sys.argv.(1) |> List.to_string ~start:"" ~stop:"" ~sep:"" id
let tree = Front.lex term_str |> Front.parse_prog

let rec p (t : Front.term) : unit =
  match t with
  | Name (_, n) -> print_string n
  | App (_, t1, t2) -> print_string "(" ; p t1 ; print_string " " ; p t2 ; print_string ")"
  | Abstr (_, n, t) -> print_string "λ" ; p n ; print_char '.' ; p t

let _ = p tree ; print_endline ""
