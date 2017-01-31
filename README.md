# Aero-OCaml - Written Spring 2016

A functional programming language written using OCamllex/yacc.

Aero shares many aspects with OCaml, for example Aero also uses eager evaluation for assignment and function calls (Evaluating the expression at the time of assignment rather than when it’s used). However, if an empty function is used (one that takes no arguments), lazy evaluation can be simulated. This is very similar to languages such as scheme. It is also a mostly functional language, but features such as Loops and Projection have been added for programmer convenience. Due to the presence of recursive functions, the language is also turing complete.

Aero uses the idea of Closures to evaluate functions. Functions are ‘reduced’ to Closures, which means when a Closure is applied, the parameters passed in will be binded to the ‘local’ environment the closure is evaluated in (which is the body of the function). The value the function returns is then returned back into the ‘global’ environment. A Closure is represented as <fun> in string form.
An expression <expr> can be either of the form of one expression, or multiple expressions surrounded by braces and separated by a semicolon. An example would be 

{print (l1.x); printnl 0;} 

A variable <var> is an element of <expr>, contains any valid string which does not start in a number and only uses numbers, letters and ‘_’ within it.

Semicolons ‘;;’ are used to separate the main statements in the body of a program. Without these a Syntax error will be thrown. The full Syntax is shown below, along with example programs and code.
The base types, called literals, are Bool, Integer and List. Every <expr> will evaluate a literal.

Example Code:
  | Project (e, i) ->
      (match eval env e with
	      List vs -> 
        (match eval env i with
            | Val v1 -> 
               (try eval env (List.nth vs v1) with (Failure "nth") -> 
                runtimeerr ("The index given is out of bounds, given " ^ exprString (eval env i) ^ " but the list is of size " ^ string_of_int (List.length vs)))
            | _ -> runtimeerr ("A int was expected, received " ^ exprString (eval env i)))
	 | _ -> runtimeerr ("A list was expected, received " ^ exprString (eval env e)))
   
Example of for loop code:
for foo (length l1) do
if foo =0
	then {print (l1.foo; printnl 0;}
	else {print (l1.foo); printnl (l1.(foo-1));};;
Example Cons code:
[0, 1, 2]::1 = [1, 0, 1, 2];;
Example Project code:
[0, 1, 2].1 = 1;;
Example Tail code:
tl [0, 1, 2] = [1, 2];;
Example Head code:
hd [0, 1, 2] = 0;;
Example Match code:
match [0, 1, 2] with 
	[] : 0
        || h, t : h+2;;
Example If code: 
if x=0 then 0 else x + 20;;
Example Function code:
fun v (a, b) a + b;;
Example Let code:
let x = [0, 1, 2, 3];;
Example Stream code:
Import stream0 0;;
