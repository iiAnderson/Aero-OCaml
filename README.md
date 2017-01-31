# Aero-OCaml - Written Spring 2016

A functional programming language written using OCamllex/yacc.

Aero shares many aspects with OCaml, for example Aero also uses eager evaluation for assignment and function calls (Evaluating the expression at the time of assignment rather than when it’s used). However, if an empty function is used (one that takes no arguments), lazy evaluation can be simulated. This is very similar to languages such as scheme. It is also a mostly functional language, but features such as Loops and Projection have been added for programmer convenience. Due to the presence of recursive functions, the language is also turing complete.

Aero uses the idea of Closures to evaluate functions. Functions are ‘reduced’ to Closures, which means when a Closure is applied, the parameters passed in will be binded to the ‘local’ environment the closure is evaluated in (which is the body of the function). The value the function returns is then returned back into the ‘global’ environment. A Closure is represented as <fun> in string form.
An expression <expr> can be either of the form of one expression, or multiple expressions surrounded by braces and separated by a semicolon. An example would be 

{print (l1.x); printnl 0;} 

A variable <var> is an element of <expr>, contains any valid string which does not start in a number and only uses numbers, letters and ‘_’ within it.

Semicolons ‘;;’ are used to separate the main statements in the body of a program. Without these a Syntax error will be thrown. The full Syntax is shown below, along with example programs and code.
The base types, called literals, are Bool, Integer and List. Every <expr> will evaluate a literal.

