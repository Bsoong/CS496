type 'a btree = Empty | Node of 'a*'a btree* 'a btree

let t = Node(44,Node(30, Empty, Empty), Node(72,Node(54, Empty Empty), Empty))

let rec find : 'a -> 'a btree -> bool = fun k t ->
  match t with
  |Empty -> false
  |Node(a,b,c) when a = k -> true
  |Node(a,b,c) ->
    if k < a then find k b
    else find k c

let rec add : 'a -> 'a btree -> 'a btree = fun k t ->
  match t with
  |Empty -> Node(k, Empty, Empty)
  |Node(a,b,c) when a = k -> t
  |Node(a,b,c) ->
    if k < a then Node(a, add k b, c)
    else Node(a, b, add k c)


let rec delete : 'a -> 'a btree -> 'a btree  = fun k t ->
  match t with
  |Empty -> failwith "N/A"
  |Node(a, Empty, Empty) when a = k -> Empty
  |Node(a,b, Empty) when k = a -> b
  |Node(a, Empty,c) when k = a -> c
  |Node(a,b,c) when i = k ->
    let m = max b
      in Node(m,delete m b ,c)
  |Node(a ,b ,c) ->
    if k < a
    then Node(a, delete k b, c)
    else Node(a, b, delete k c)


let rec max : 'a btree -> 'a = fun t ->
  match t with
  |Empty -> failwith "N/A"
  |Node(a,_, Empty) -> a
  |Node(_,_,c) -> max c
