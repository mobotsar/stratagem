open! Containers

let substr s a b = String.drop a s |> String.take (b - a)

type token = Name of string | Dot | Lambda | OpenParen | CloseParen | Space

let lambda_char = '#'

let tts = function
  | Name s -> s
  | Dot -> "."
  | Lambda -> Char.to_string lambda_char
  | OpenParen -> "("
  | CloseParen -> ")"
  | Space -> " "

let generic = function
  | '.' -> Dot
  | c when Char.equal c lambda_char -> Lambda
  | '(' -> OpenParen
  | ')' -> CloseParen
  | ' ' -> Space
  | c -> raise (Failure ("illegal character (this is impossible): " ^ Char.to_string c))

let name = Str.regexp {|[a-zA-Z]+|}
let valid_not_name = Str.regexp ({|[\.() |} ^ Char.to_string lambda_char ^ "]")

let lex s =
  let rec lex' s debut fin toks =
    if debut >= fin then toks
    else if Str.string_match valid_not_name s debut then
      lex' s (debut + 1) fin (generic s.[debut] :: toks)
    else if Str.string_match name s debut then
      let next : int = Str.match_end () in
      lex' s next fin (Name (substr s debut next) :: toks)
    else raise (Failure ("illegal character: " ^ Char.to_string s.[debut]))
  in
  lex' s 0 (String.length s) [] |> List.rev

(* type term = Name | Abstraction of term * term | Application of term * term *)
