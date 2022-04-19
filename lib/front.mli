type token = NameTok of string | Dot | Lambda | OpenParen | CloseParen | Space | End
type info = { loc : int * int }
type term = Name of info * string | Abstr of info * term * term | App of info * term * term
val parse_prog : token list -> term
val tts : token -> string
val lex : string -> token list
(* val lambda_char : cha *)
