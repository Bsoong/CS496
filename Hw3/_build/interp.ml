(*I Pledge my honor that I have abided by the Stevens Honor System -Bsoong*)
open Ast
open Ds

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  (*Subtraction function*)
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  (*Addition function*)
  | Add(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal((numVal_to_num v1) + (numVal_to_num v2))
  (*Division function*)
  | Div(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal((numVal_to_num v1) / (numVal_to_num v2))
  (*Multiplication function*)
  | Mul(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal((numVal_to_num v1) * (numVal_to_num v2))
  (*Abs value function*)
  | Abs(e1)         ->
    let v1 = eval en e1 in
    NumVal(abs (numVal_to_num v1))
  (*Cons function in the interpreter*)
  | Cons(e1, e2)    ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    (match v2 with
     |ListVal xs -> ListVal(v1::xs)
     |_ -> failwith "e2 is not a listVal")
  (*Head function that returns the head element*)
  | Hd(e1)          ->
   let v1 = eval en e1 in
   List.hd (listVal_to_list v1)
 (*Returns the tail element*)
  | Tl(e1)          ->
    let v1 = eval en e1 in
    ListVal(List.tl (listVal_to_list v1))
  (*Checks specific type values and if they're empty*)
  | Empty(e1)       ->
    let v1 = eval en e1 in (* supports both lists and trees *)
    (match v1 with
     |ListVal [] -> BoolVal true
     |ListVal _ -> BoolVal false
     |TreeVal Empty -> BoolVal true
     |TreeVal _ -> BoolVal false
     |_ -> failwith("Expected either list or tree"))
  | EmptyList       -> ListVal([])
  | EmptyTree       -> TreeVal(Empty)
  | Node(e1,lt,rt)  ->
    let v1 = eval en e1 in
    let l = eval en lt in
    let r = eval en rt in
    (match l, r with
     |TreeVal left, TreeVal right -> TreeVal(Node(v1, left,  right))
     |_ , _ -> failwith("Expected Treeval")
     )
  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) ->
    (match eval en target with
     |TreeVal Empty -> eval en emptycase
     |TreeVal Node(x, lt, rt) ->  eval (extend_env (extend_env (extend_env en id_e x) id_lt (TreeVal lt)) id_rt (TreeVal rt)) nodecase
     |_ -> failwith("Expecting a tree"))



(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
