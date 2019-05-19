(*I pledge my Honor that I have abided by the Stevens Honor System -Bsoong*)
open Ast

let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)

type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv

let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)


let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) ->
    if id=key
    then Some value
    else apply_tenv tenv1 id


let init_tenv () =
     extend_tenv "x"  IntType
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env



let rec type_of_prog = function
  | AProg e -> type_of_expr (init_tenv ()) e
and
  type_of_expr en = function
  | Int n          -> IntType
  | Var id          ->
    (match apply_tenv en id with
    | None -> failwith @@ "Variable "^id^" undefined"
    | Some texp -> texp)
  | Unit ->
    UnitType
  | ITE(e1, e2, e3)    ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in let t3 = type_of_expr en e3
    in if t1=BoolType && t2=t3
    then t2
    else failwith "ITE: Type error"
  | Add(e1, e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2)    ->
    let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2  in
    if t1=IntType && t2=IntType
    then IntType
    else failwith "Add: arguments must be ints"
  | IsZero(e) ->
    let t1 = type_of_expr en e  in
    if t1=IntType
    then BoolType
    else failwith "Zero?: argument must be int"
  | Let(x, e1, e2) ->
    let t1 = type_of_expr en e1
    in type_of_expr (extend_tenv x t1 en) e2
  | Proc(x,ty,e)      ->
    let tc= type_of_expr (extend_tenv x ty en) e
    in FuncType(ty,tc)
  | App(e1,e2)     ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in (match t1 with
    | FuncType(td,tcd) when td=t2 -> tcd
    | FuncType(td,tcd) -> failwith "App: argument does not have correct type"
    | _ -> failwith "Checker: App: LHS must be function type")
  | Letrec(tRes,id,param,tParam,body,e) ->
    let t=type_of_expr (extend_tenv param tParam
                          (extend_tenv id (FuncType(tParam,tRes)) en))
        body
    in if t=tRes
    then type_of_expr (extend_tenv id (FuncType(tParam,tRes)) en) e
    else failwith
        "Checker: LetRec: Types of recursive function does not match declaration"
  | Set(id,e) ->
      failwith "EXPLICIT-REFS: Set not a valid operation"
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es

  (* explicit ref *)
  | NewRef(e) ->
    (match type_of_expr en e with
     | t -> RefType(t))

  | DeRef(e) ->
    (match type_of_expr en e with
     |RefType(t) -> t
     |_ -> failwith "Error expected RefType")

  | SetRef(e1,e2) ->
    (match (type_of_expr en e1), (type_of_expr en e2) with
     | RefType(t), x when t = x  -> UnitType
     | _ -> failwith "Error expected UnitType")

  (* pair *)
  | Pair(e1, e2) ->
    let t1  = type_of_expr en e1 in
    let t2 = type_of_expr en e2 in
    PairType(t1, t2)
  | Unpair(id1, id2, def, body) ->
    (match type_of_expr en def with
     |PairType(x,y) ->
       let tenv = extend_tenv id1 x (extend_tenv id2 y en ) in type_of_expr tenv body
     |_ -> failwith "Error Expected PairType")
  (* list *)
  | EmptyList(t) -> ListType(t)
  | Cons(he, te) ->
    (match (type_of_expr en he),(type_of_expr en te) with
      |t,ListType(x) -> ListType(x)
      |_ ->  failwith "Error types do not match up"
    )
  | Null(e) ->
    (match type_of_expr en e with
     | ListType(t) -> BoolType
     | _ -> failwith "Error types do not matchup")
  | Hd(e) ->
    (match type_of_expr en e with
     | ListType(t) -> t
     |_ -> failwith "Error types do not matchup")

  | Tl(e) ->
    (match type_of_expr en e with
     | ListType(t) -> ListType(t)
     |_ -> failwith "Error types do not matchup")

  (* tree *)
  | EmptyTree(t) -> TreeType(t)
  | Node(de, le, re) ->
    let t1 = type_of_expr en de in
    let t2 = type_of_expr en le in
    let t3 = type_of_expr en re in
    (match t2, t3 with
     | TreeType(x), TreeType(y) when x = y && x = (t1) -> TreeType(x)
     |_ -> failwith "Error types do not matchup")
  | NullT(t) ->
    (match type_of_expr en t with
     |TreeType(t) -> BoolType
     |_ -> failwith "Error Expected TreeType")
  | GetData(t) ->
    (match type_of_expr en t with
     |TreeType(t) -> t
     |_ -> failwith "Error Expected TreeType")
  | GetLST(t) ->
    (match type_of_expr en t with
     |TreeType(t) -> TreeType(t)
     |_ -> failwith "Error Expected TreeType")
  | GetRST(t) ->
    (match type_of_expr en t with
     |TreeType(t) -> TreeType(t)
     |_ -> failwith "Error Expected TreeType" )
  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv en;
    UnitType

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let chk (e:string) : texpr =
  e |> parse |> type_of_prog
