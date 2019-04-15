type 'a bnode  = { data: 'a;
                   mutable left:  bnode option;
                   mutable right: bnode option;}
type 'a btree = { mutable root: bnode option }

let empty_btree : unit -> 'a btree =
  {root = None}

let join_btree : 'a btree -> 'a btree -> 'a -> 'a btree = fun lt rt d ->
  {root = Some {data = d; left = lt.root; right = rt.root}}
