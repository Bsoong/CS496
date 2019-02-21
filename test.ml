let rec even n =
match n with
| 0 -> true
| n -> odd (n-1)
and
odd n =
match n with
| 0 -> false
| n -> even (n-1)

let rec gtz = function
|[] -> []
| x::xs ->
  if x >0
  then gtz xs
  else x::gtz xs

let rec suml : int list -> int = function
  |[] -> 0
  | x::xs -> x + suml xs

let rec andl : bool list -> bool = function
  |[] -> true
  |x::xs -> x && andl xs
let rec concat : 'a list list -> 'a list = function
  | [] -> []
  |xs::xss -> xs @ concat xss
let rec filter: ( 'a -> bool)  -> 'a list -> 'a list   = fun p xs ->
match xs with
| [] -> []
| y::ys ->
  if p y
  then y:: filter p ys
  else filter p ys
let rec fold = fun f a-> function
  | [] -> a
  | x::xs -> f x (fold f xs)

let rec foldr = fun f a xs ->
  match xs with
  |[] -> a
  |x::xs -> foldr f (f a x) xs

let rec foldl :('a -> 'b  -> 'b) -> 'b -> 'a list -> 'b = fun f a xs ->
  match xs with
  |[] -> a
  |x::xs -> f x (foldl f a xs)
