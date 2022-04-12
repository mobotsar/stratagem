open Stratagem
open Util

let ts = Front.lex "#barney.#c.barney (c) (#t.#f.f)"

let () = print_endline @@ Ls.sol Front.tts ts

let () = print_endline "It's not broken yet!"