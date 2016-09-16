open AeroSyntax

exception RuntimeErr of string

let runtimeerr msg = raise (RuntimeErr msg)

let rec eval env = function
  | Var x -> (try List.assoc x env with Not_found -> runtimeerr ("The variable " ^ x ^ " is not bound"))

  | Val _ as e -> e | Const _ as e -> e

 (**Arithmetic operators*)
  | Plus (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
	   Val int1, Val int2 -> Val (int1 + int2)
	 | _, _ -> runtimeerr ("Expected Integer + Integer, received " ^ exprString v1 ^ " + " ^ exprString v2))
 
  | Minus (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Val int1, Val int2 -> Val (int1 - int2)
   | _, _ -> runtimeerr ("Expected Integer - Integer, received " ^ exprString v1 ^ " - " ^ exprString v2))
 
  | Times (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Val int1, Val int2 -> Val (int1 * int2)
   | _, _ -> runtimeerr ("Expected Integer * Integer, received " ^ exprString v1 ^ " * " ^ exprString v2))
 
  | Divide (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
	   Val int1, Val 0 -> runtimeerr "A Division by Zero has occured"
	 | Val int1, Val int2 -> Val (int1 / int2)
   | _, _ -> runtimeerr ("Expected Integer / Integer, received " ^ exprString v1 ^ " / " ^ exprString v2))
 
  | Increment (e1) ->
    let v1 = eval env e1 in
    (match v1 with
      Val v -> Val(v+1)
    | _ -> runtimeerr ("Expected Integer, received " ^ exprString v1))

  | Decrement (e1) ->
    let v1 = eval env e1 in
    (match v1 with
      Val v -> Val(v-1)
    | _ -> runtimeerr ("Expected Integer, received " ^ exprString v1))

 (** BOOLEAN OPERATORS*)
  | Equal (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Val int1, Val int2 -> Const (int1 = int2)
   | _, _ -> runtimeerr ("Expected Integer = Integer, received " ^ exprString v1 ^ " = " ^ exprString v2))

  | Less (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
	   Val int1, Val int2 -> Const (int1 < int2)
   | _, _ -> runtimeerr ("Expected Integer < Integer, received " ^ exprString v1 ^ " < " ^ exprString v2))
 
  | Greater (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Val int1, Val int2 -> Const (int1 > int2)
   | _, _ -> runtimeerr ("Expected Integer > Integer, received " ^ exprString v1 ^ " > " ^ exprString v2))
 
  | And (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Const b1, Const b2 -> Const (b1 && b2)
   | _, _ -> runtimeerr ("Expected Bool && Bool, received " ^ exprString v1 ^ " && " ^ exprString v2))
 
  | Or (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in 
      (match v1, v2 with
     Const b1, Const b2 -> Const (b1 || b2)
   | _, _ -> runtimeerr ("Expected Bool || Bool, received " ^ exprString v1 ^ " || " ^ exprString v2))
 
  | Not b ->
      (match eval env b with
	   Const b -> Const (not b)
	 | _ -> runtimeerr ("Expected not Bool, received " ^ exprString (eval env b)))
  
  (**Expression ops*)
  | If (e1, e2, e3) ->
      (match eval env e1 with
	   Const true -> eval env e2
	 | Const false -> eval env e3
	 | _ -> runtimeerr ("Boolean value expected in conditional, received " ^ exprString (eval env e1)))
 
  | Fun (fname, varlst, e) ->
      let rec recursive = Closure ((fname,recursive)::env, varlst, e) in recursive

  | Closure _ as e -> e
 
 (**Define operators*)
  | Let (var, e1, e2) -> eval ((var, eval env e1)::env) e2
 
  | App (e1, elist) ->
      (match eval env e1 with
	   Closure (newenv, lst, e) -> 
		try
          	 let rec addEnv env newenv elst lst = (match lst with
          	   | [] -> newenv 
            	   | h::t -> addEnv env ((h, eval env (List.hd elst))::newenv) (List.tl elst) t 
		   | _ -> newenv) in
       		eval (addEnv env newenv elist lst) e
		with Failure("tl") -> runtimeerr ("The function takes " ^ String.concat ", " (List.map (fun e -> ""^ e) lst) ^ " ("^string_of_int (List.length lst)^" parameters)," ^" but was passed " ^ String.concat ", " (List.map (fun e -> ""^ exprString e) elist) ^ " ("^string_of_int (List.length elist)^" parameters)") 
	 | _ -> runtimeerr "Incorrect use of <fun>")

 (**List operators*)
  | List rs ->
      List (List.map (fun e -> eval env e) rs)
                       
  | Project (e, i) ->
      (match eval env e with
	      List vs -> 
        (match eval env i with
            | Val v1 -> 
               (try eval env (List.nth vs v1) with (Failure "nth") -> 
                runtimeerr ("The index given is out of bounds, given " ^ exprString (eval env i) ^ " but the list is of size " ^ string_of_int (List.length vs)))
            | _ -> runtimeerr ("A int was expected, received " ^ exprString (eval env i)))
	 | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env e)))
 
  | Cons (e1, e) ->
    (match eval env e1 with
      List l -> List ((eval env e)::l)
    | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env e1)))
  
(*   | Replace (var, e) ->
      env = List.filter (fun (x, y) -> x<>var) env  ; List.assoc var env; *)

  | Length (l) ->
    (match eval env l with
      List l-> Val (List.length l)
    | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env l)))
 
  | Tail (l) ->
    (match eval env l with
      List l -> List(List.tl l)
      | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env l)))

  | Head (l) ->
    (match eval env l with
      List l -> List.hd l
      | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env l)))

  | Match (e, e1, var1, var2, e2) ->
      let lst = eval env e in
      (match lst with
        | List([]) -> eval env e1
        | List(lst) -> eval ((var1, List.hd lst)::(var2, List(List.tl lst))::env) e2
        | _ -> runtimeerr ("Did not match any cases "^ exprString (eval env e)))

    (* Print Operators *)
  | Print (e) ->
     (match eval env e with
      | Val n -> print_string ((string_of_int n) ^ " "); Val(n)
      | Const b -> print_string ((string_of_bool b) ^ " "); Const(b)
      | List rs ->
           print_string (String.concat "\n" (List.map (fun e -> ""^ (exprString e)) (List.rev rs)) ^"\n") ; List rs
      | _ -> runtimeerr ("A expr was expected, received " ^ exprString (eval env e)))
 
  | PrintString (str, e)->
  (match eval env e with
      | Val n -> print_string (str^ " " ^ (string_of_int n) ^ " "); Val(n)
      | Const b -> print_string (str^ " " ^ (string_of_bool b) ^ " "); Const(b)
      | List rs ->
           print_string ( "{" ^ String.concat ", " (List.map (fun e -> ""^ (exprString e)) rs) ^ "} ") ; List rs
      | _ -> runtimeerr ("A expr was expected, received " ^ exprString (eval env e)))

  | PrintNewLine (e) ->
     (match eval env e with
      | Val n -> print_endline (string_of_int n); Val(n)
      | Const b -> print_endline (string_of_bool b); Const b
      | List rs ->
          print_endline ( "{" ^ String.concat ", " (List.map (fun e -> ""^ (exprString e)) rs) ^ "} ") ; List rs
      | _ -> runtimeerr ("A expr was expected, received " ^ exprString (eval env e)))

(* Will return the last executed command in the body *)
  | Body (e) ->
          let rec lst myList = (match myList with
          | h::[] -> (eval env h)
          | h::b -> (eval env h); lst b) in
          lst e

(* Iterative operators *)
  | For(var, e, e1) -> 
    let v = eval env e in
    (match v with
      Val v1 ->
         for i = 0 to v1-1 do 
         (eval ((var,Val(i))::env) e1)
         done; Const true
      | _ -> runtimeerr ("A int was expected, received " ^ exprString v))

  | While (b, e) ->
    let b1 = eval env e in
    (match b1 with
      Const b2 ->
          (match b2 with
            | true -> While(b, (eval env e))
            | false -> Const(true))
    | _ -> runtimeerr ("A bool was expected, received " ^ exprString b1))
