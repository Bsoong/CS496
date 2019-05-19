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


let rec ne = function
| [] -> []
| x :: xs ->
  if x = []
  then x:: new xs
  else new xs

let rec filter: ( 'a -> bool)  -> 'a list -> 'a list   = fun p xs ->
match xs with
| [] -> []
| y::ys ->
  if p y
  then y:: filter p ys
  else filter p ys
