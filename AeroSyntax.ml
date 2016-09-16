type variablename = string

type literal =
  | Integer
  | Bool
  | Apply of literal * literal
  | ListType of literal list

  type expr =
  (* Type wrappers *)
  | Var of variablename
  | String of string
  | Val of int
  (* Arithmetic operators *)
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Increment of expr
  | Decrement of expr
  (* Boolean operators *)
  | Const of bool
  | Equal of expr * expr
  | Less of expr * expr
  | Greater of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  (* Conditionals *)
  | If of expr * expr * expr
  (* Functions and assingment *)
  | Fun of variablename * variablename list * expr
  | Closure of env * variablename list * expr
  | Let of variablename * expr * expr
  | App of expr * expr list
  (* List operators *)
  | List of expr list
  | Project of expr * expr 
  | Cons of expr * expr
  | Length of expr
  | Tail of expr
  | Head of expr
  | Match of expr * expr * variablename * variablename * expr
  (* Print operators *)
  | Print of expr
  | PrintNewLine of expr 
  | PrintString of variablename * expr
  | Body of expr list
  (* Iteration operator *)
  | For of variablename * expr * expr
  | While of expr * expr

and env = (variablename * expr) list

type mainBody =
    | Expr of expr
    | Define of variablename * expr
    | Stream of variablename * int

(* Used for testing purposes *)
let rec exprString = function
  | Val n -> string_of_int n
  | Plus (n, n1) -> exprString n ^ " + " ^ exprString n1 
  | Var (var) -> var
  | Body (e) -> "{" ^ String.concat ", "
       (List.map (fun e -> ""^ (exprString e)) e) ^
       "}"
  | If (c, e1, e2) -> "if " ^ exprString c ^ " then " ^ exprString e1 ^ " else " ^ exprString e2 
  | Const b -> string_of_bool b
  | List rs ->
      "{" ^ String.concat ", " (List.map (fun e -> ""^ (exprString e)) rs) ^ "}"
  | Closure _ -> "<fun>"
  | _ -> ""
