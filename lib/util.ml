include Containers

(* Aviary combinators: *)
let ( << ) f g x = g x |> f
let ( >> ) f g x = f x |> g
let psi f g x y = f (g x) (g y)

let lines_of file =
  let lines_ref = ref [] in
  let ic = open_in file in
  let () =
    try
      while true do
        lines_ref := input_line ic :: !lines_ref
      done
    with End_of_file -> close_in ic
  in
  List.rev !lines_ref

module type Stringable = sig
  type t
  val to_string : t -> string
end

let rec applied_n_times_to f n x = if n = 0 then x else applied_n_times_to f (n - 1) (f x)

external id : 'a -> 'a = "%identity"

let ( >< ) a b =
  let rec aux n ac = if n < a then ac else aux (n - 1) (n :: ac) in
  aux b []

let flip f a b = f b a

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub

let tup x y = x, y

module Ls = struct
  include List

  let sum = fold_left ( + ) 0

  let product = fold_left ( * ) 1

  let sol to_string l = "[" ^ (List.map to_string l |> String.concat "; ") ^ "]"

  let rec map2_default f xd yd x y =
    match x, y with
    | [], [] -> []
    | x :: xs, [] -> f x yd :: map2_default f xd yd xs []
    | [], y :: ys -> f xd y :: map2_default f xd yd [] ys
    | x :: xs, y :: ys -> f x y :: map2_default f xd yd xs ys

  let rec map2_truncate f x y =
    match x, y with
    | [], _ | _, [] -> []
    | x :: xs, y :: ys -> f x y :: map2_truncate f xs ys

  let rec unzip_with f = function
    | [] -> [], []
    | x :: xs ->
        let a, b = f x in
        let xs' = unzip_with f xs in
        a :: fst xs', b :: snd xs'

  let group_by projection =
    let rec aux = function
      | [] -> []
      | x :: _ as xs ->
          let key = projection x in
          let matches, next = partition (( = ) key << projection) xs in
          (key, matches) :: aux next
    in
    aux >> rev

  let uniq ls =
    let seen = Hashtbl.create @@ length ls in
    filter
      (fun x ->
        let tmp = Hashtbl.mem seen x |> not in
        Hashtbl.replace seen x () ; tmp )
      ls

  let rec transpose = function
    | (x :: xs) :: xss -> (x :: map hd xss) :: transpose (xs :: map tl xss)
    | _ :: xss -> transpose xss
    | _ -> []
end

module Fn = struct
  include Fun
  module Aviary = struct
    let idiot_bird = id
    let applicator_bird = ( @@ )
    let kestrel a _ = a
    let kite _ b = b
    let bluebird = ( << )
    let bluebird' f x g y = f x (g y)
    let cardinal f x y = f y x
    let psi f g x y = f (g x) (g y)
    let blackbird f g x y = g x y |> f
    let bunting f g x y z = g x y z |> f
    let becard f g h x = f (g (h x))
    let starling f g x = f x (g x)
    let starling' f g h x = f (g x) (h x)
    let missing_bird f g x = f (g x) x
    let phoenix = starling'
    let cardinal' f g x y = f (g y) x
    let cardinal_star f x y z = f x y z
    let cardinal_star_star f s t u v = f s t u v
    let eagle f x g y z = f x (g y z)
    let dickcissel f x y g z = f x y (g z)
    let dovekie f g x h y = f (g x) (h y)
    let bald_eagle f g s t h u v = f (g s t) (h u v)
    let finch x y f = f y x
    let finch_star f x y z = f z y x
    let finch_star_star f s t u v = f s v u t
    let goldfinch f g x y = f y (g x)
    let hummingbird f x y = f x y x
    let idiot_star f x = f x
    let idiot_star_star f x y = f x y
    let jay f x y z = f x (f z y)
    let owl x y = y (x y)
    let quacky x f g = g (f x)
    let queer = ( >> )
    let quirky f x g = g (f x)
    let quixotic f x g = f (g x)
    let quizzical x f g = f (g x)
    let robin x f y = f y x
    let robin_star f x y z = f y z x
    let robin_star_star f s t u v = f s u v t
    let thrush x f = f x
    let vireo x y f = f x y
    let vireo_star f x y z = f y x z
    let vireo_star_star f s t u v = f s v t u
    let warbler f x = f x x
    let warbler1 x f = f x x
    let warbler_star f x y = f x y y
    let warbler_star_star f x y z = f x y z z
  end
end
