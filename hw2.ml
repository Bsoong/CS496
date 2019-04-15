(*I pledge my honor that I have abided by the Stevens Honor System-Bsoong*)
type dtree =
  |Leaf of int
  |Node of (char * dtree * dtree)

let dleft = Node('w', Node('x', Leaf(2),Leaf(5)), Leaf(8))

let dright = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

let dmid = Node('w', Node('x',Node('y', Leaf(0), Leaf(0)), Node('y', Leaf(0), Leaf(0))), Node('y', Node('y', Leaf(0), Leaf(0)), Leaf(0)))

let rec dTree_height t =
  match t with
  |Leaf(x) -> 0
  |Node(i, lt, rt) ->
    let l = dTree_height lt in
    let r = dTree_height rt in
    if l > r then l + 1 else r + 1

let rec dTree_size t =
  match t with
  |Leaf(x) -> 1
  |Node(i,lt,rt) -> (dTree_size lt) + (dTree_size rt) + 1

let rec dTree_paths t =
let helperZero lst =
  0::lst in
let helperOne lst =
  1::lst in
  match t with
  |Leaf(x) -> [[]]
  |Node(i, lt, rt) -> (List.map helperZero (dTree_paths lt)) @ (List.map helperOne (dTree_paths rt))

let rec dTree_is_perfect t =
  match t with
  |Leaf(x) -> true
  |Node(i,lt,rt) ->
    let l = dTree_height lt in
    let r = dTree_height rt in
    if l != r then false else (dTree_is_perfect lt) && (dTree_is_perfect rt)

let rec dTree_map f g t =
  match t with
  |Leaf(x) -> Leaf(g x)
  |Node(i, lt, rt) -> Node(f i, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree lst =
  match lst with
  |[] -> Leaf(0)
  |head::tail ->
    match head with
    |lst -> Node(head, list_to_tree tail, list_to_tree tail)

let rec replace_leaf_at  t g =
  let rec replaceleaf_helper t r num =
    match r with
    |[] -> Leaf(num)
    |y::ys ->
      match t with
      |Node(i, lt, rt) ->
        if y = 0
        then Node(i, replaceleaf_helper lt ys num, rt)
        else Node(i, lt, replaceleaf_helper rt ys num)
      |_ -> failwith "Error out"
  in match g with
  |[] -> t
  |(x,y)::xs -> replace_leaf_at (replaceleaf_helper t x y) xs

let rec bf_to_dTree (x,y) =
  replace_leaf_at (list_to_tree x) y
