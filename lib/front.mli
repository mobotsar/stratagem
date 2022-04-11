type token = Name of string | Dot | Lambda | OpenParen | CloseParen | Space
val tts : token -> string
val lex : string -> token list
