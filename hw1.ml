
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]

let a3 = {states = ["q0";"q1";"q2";"q3"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q0",'a',"q3"); ("q1",'b',"q1");("q1",'c',"q2")];
          final = ["q3";"q2"];
         }
let a4 = {states = ["q0";"q1";"q2";"q3";"q4";"q5"; "q6"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q0",'c',"q2");  ("q0",'a',"q4"); ("q0", 'a',"q5"); ("q0", 'a',"q6")];
          final= ["q6"]
         }
let a5 = {
  states = ["q0";"q1";"q2";"q3";"q4";"q5"; "q6";"q7";"q8"];
  start = "q0";
  tf = [
    ("q0",'a',"q1");
    ("q1", 'b', "q0");
    ("q1",'c',"q2");
    ("q2", 'd', "q1");
    ("q1",'e',"q4");
    ("q4", 'f',"q1");
    ("q4", 'g',"q3");
    ("q3", 'h', "q4");
    ("q3", 'i', "q2");
    ("q2",'j', "q3");
    ("q1", 'k', "q5");
    ("q5", 'l', "q1");
    ("q0", 'm' ,"q2");
    ("q2", 'n', "q0");
    ("q3", 'o', "q6");
    ("q6", 'p', "q3");
    ("q7", 'q', "q8");
    ("q8",'r', "q7");];
  final = ["q6"];
}

(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(*helper function for remove_dead_states*)
let rec remove_state state stateC =
  match state with
  |[] -> []
  |x::xs ->
    if List.mem x stateC
    then x::remove_state xs stateC
    else remove_state xs stateC

(*helper function for remove_dead_states*)
let rec remove_tf tf stateA =
  match tf with
  |[] -> []
  |(q,w,q')::tf' ->
    if List.mem q stateA
    then (q,w,q')::remove_tf tf' stateA
    else remove_tf tf' stateA

(*Helper function that combines two lists*)
let rec m list1 list2 =
  match list1 with
  |[] -> list2
  |x::xs -> if List.mem x list2
      then m xs list2
      else x::m xs list2

(*helper function for deterministic that pushed the fa to the "next" state*)
let rec next tf state symbol =
  match tf with
  |[] -> []
  |(q,w,q')::tf' ->
    if q = state && w = symbol
    then q'::next tf' state symbol
    else next tf' state symbol

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(*apply_transition function that recursively calls itself to traverse through the FA*)
let rec apply_transition_function tf state sym =
  match tf  with
  | [] -> None
  | (q,w,q')::tf' when q = state && w = sym -> Some q'
  | _ :: tf' -> apply_transition_function tf' state sym

(*The Accept function using an accept helper to manipulate the states. Then recursively calls itself in accept *)
let rec accept fa input =
  let rec accHelp state input =
    match input with
    |[] -> state
    |sym::symbs ->
      match state with
      |Some q -> accHelp (apply_transition_function fa.tf q sym) symbs
      |None -> None
  in match accHelp(Some fa.start) input with
  |None -> false
  |Some state -> List.mem state fa.final

(*Deterministic function that uses a helper to check if a given fa is true or false*)
let rec deterministic fa =
  let rec det_help = function
    | [] -> true
    | (q,w,q') :: tf ->
      match next tf q w with
      | [] -> det_help tf
      | _ -> false
  in det_help fa.tf

(*Boolean function valid that checks the start state, final state, and if the fa is determistic*)
let rec valid fa =
  let is_state state = List.mem state fa.states
  in is_state fa.start && List.for_all is_state fa.final && deterministic fa

let rec reachable fa =
  let rec reach_help tf start states =
    match tf with
    |[] -> []
    |(q,w,q')::tf' ->
      if q = start && q <> q'
      then
          if List.mem q' states
          then reach_help tf' start states
          else q'::reach_help tf' start (q'::states)
      else reach_help tf' start states
  in let rec lo state stateA =
       match state with
       |[] -> [fa.start]
       |x::xs -> if List.mem x stateA
         then lo xs stateA
         else x::lo(m(reach_help fa.tf x(List.append state stateA)) xs) (List.append [x] stateA)
  in lo (reach_help fa.tf fa.start []) [fa.start]

let rec remove_dead_states fa =
  let stateA = reachable fa in
  {
    states = stateA;
    tf = remove_tf fa.tf stateA;
    start = fa.start;
    final = remove_state fa.final stateA;
  }
