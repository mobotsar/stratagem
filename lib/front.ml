open! Containers

type token = NameTok of string | Dot | Lambda | OpenParen | CloseParen | Space | End

let lambda_char = '#'

let tts = function
  | NameTok s -> s
  | Dot -> "."
  | Lambda -> Char.to_string lambda_char
  | OpenParen -> "("
  | CloseParen -> ")"
  | Space -> " "
  | End -> "|"

let generic = function
  | '.' -> Dot
  | c when Char.equal c lambda_char -> Lambda
  | '(' -> OpenParen
  | ')' -> CloseParen
  | ' ' -> Space
  | '|' -> End
  | c -> raise (Failure ("illegal character (this is impossible): " ^ Char.to_string c))

let name = Str.regexp {|[a-zA-Z_]+|}
let valid_not_name = Str.regexp ({|[\.() |} ^ Char.to_string lambda_char ^ "]")

let rec normalize (mylist : 'a list) : 'a list =
  match mylist with
  | Space :: (Space :: _ as xs) -> normalize xs
  | x :: xs -> x :: normalize xs
  | _ -> mylist

let lex s =
  let rec lex' s debut fin toks =
    if debut >= fin then toks
    else if Str.string_match valid_not_name s debut then
      lex' s (debut + 1) fin (generic s.[debut] :: toks)
    else if Str.string_match name s debut then
      let next = Str.match_end () in
      lex' s next fin (NameTok (String.sub s debut (next - debut)) :: toks)
    else raise (Failure ("illegal character: " ^ Char.to_string s.[debut]))
  in
  (lex' s 0 (String.length s) [] |> List.rev) @ [ End ] |> normalize

type info = { loc : int * int }

let nilinf = { loc = -1, -1 }

type term = Name of info * string | Abstr of info * term * term | App of info * term * term

(*
  Term -> x
        | # S x S . S Term
        | ( S Term space Term S)
  S    -> space
        | ε
*)

exception NoRule

let parse_prog toks =
  let toks = ref toks in
  let peek () =
    match !toks with
    | [] -> raise (Failure "oof peek")
    | x :: _ -> x
  in
  let match_tok t =
    match !toks with
    | x :: xs when Stdlib.( = ) t x -> toks := xs
    | x :: _ -> raise (Failure ("oof match_tok: \'" ^ tts x ^ "\'"))
    | [] -> raise (Failure "oof match_tok: no token left")
  in
  let rec parse_term () =
    match peek () with
    | NameTok name as t ->
        match_tok t ;
        Name (nilinf, name)
    | Lambda ->
        match_tok Lambda ;
        match_optional_space () ;
        let name = peek () in
        match_tok name ;
        let name_str =
          match name with
          | NameTok name_str -> name_str
          | _ -> raise (Failure "oof parse_term")
        in
        match_optional_space () ;
        match_tok Dot ;
        match_optional_space () ;
        let m = parse_term () in
        Abstr (nilinf, Name (nilinf, name_str), m)
    | OpenParen ->
        match_tok OpenParen ;
        match_optional_space () ;
        let t1 = parse_term () in
        match_tok Space ;
        let t2 = parse_term () in
        match_optional_space () ;
        match_tok CloseParen ;
        App (nilinf, t1, t2)
    | _ -> raise NoRule
  and match_optional_space () =
    match peek () with
    | Space -> match_tok Space
    | _ -> ()
  in
  match_optional_space () ;
  let t = parse_term () in
  match_optional_space () ; match_tok End ; t

(* expression templates *)
