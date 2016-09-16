%{
  open AeroSyntax
%}

%token INTEGER BOOL APPLY FOR DO MATCH WHILE
%token LBRACE RBRACE
%token LBLOCK RBLOCK
%token LENGTH PRINT PRINTNEWLINE PRINTSTRING
%token HEAD TAIL GET
%token COLON COMMA PERIOD CONS
%token <AeroSyntax.variablename> VAR
%token <int> INT
%token PLUS MINUS TIMES DIVIDE
%token TRUE FALSE
%token EQUAL LESS GREATER INCR DECR
%token AND OR NOT
%token IF THEN ELSE WITH
%token FUN IS
%token LPAREN RPAREN
%token LET IN
%token SEMICOLON EOL QUOTE
%token EOF

%right SEMICOLON
%right COMMA
%nonassoc LET IN
%right FUN IS
%nonassoc IF THEN ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL LESS GREATER
%left PLUS MINUS
%left TIMES DIVIDE
%right INCR DECR
%left PERIOD
%right APPLY

%start mainBody
%type <AeroSyntax.mainBody list> mainBody
%type <AeroSyntax.expr list> body


%%

mainBody:
  | EOF                        { [] }
  | main                       { $1 }

main:
  | define EOF                 { [$1] }
  | define SEMICOLON mainBody  { $1 :: $3 }
  | define main                { $1 :: $2 }
  | expr EOF                   { [Expr $1] }
  | expr SEMICOLON mainBody    { Expr $1 :: $3 }
  | stream SEMICOLON mainBody  { $1 :: $3 }
  | stream EOF                 { [$1] }
  | stream main                { $1 :: $2 }

body:                          { [] }
  | line body                  { $1 :: $2 }

line:
  | expr EOL                   { $1 }

stream:
 | GET VAR INT                 { Stream ($2, $3)}

define:
  | LET VAR EQUAL expr         { Define ($2, $4) }

expr:
  | term                       { $1 }
  | application                { $1 }
  | LBRACE body RBRACE         { Body $2 }
  | arithoperators             { $1 }
  | booleanoperators           { $1 }
  | iterativeoperators         { $1 }
  | IF expr THEN expr ELSE expr	       { If ($2, $4, $6) }
  | FUN VAR LPAREN varlist RPAREN expr { Fun ($2, $4, $6) }
  | FUN VAR LPAREN RPAREN expr { Fun ($2, [], $5) }

application:
/*    application expr expr   { App ($1, $2, $3) }*/
  | expr list                { App ($1, $2) }
  | expr LPAREN RPAREN       { App ($1, [])}

varlist:
  | varelem                    { [$1] }
  | VAR COMMA varlist          { $1 :: $3 }

varelem:
  | VAR                    { $1 }

term:
    VAR		              	  { Var $1 }
  | TRUE                	  { Const true }
  | FALSE               	  { Const false }
  | INT		                  { Val $1 }
  | LPAREN expr RPAREN		  { $2 }
  | LBLOCK RBLOCK           { List [] }
  | LBLOCK list RBLOCK      { List $2 }
  | listoperators           { $1 }
  | printoperators          { $1 }

iterativeoperators:
  | FOR VAR expr DO expr    { For($2, $3, $5) }
  | WHILE expr DO expr      { While($2, $4)}

listoperators:
  | term PERIOD expr        { Project ($1, $3) }
  | LENGTH term             { Length ($2) }
  | HEAD term               { Head $2 }
  | TAIL term               { Tail $2 }
  | term CONS expr          { Cons($1, $3) }
  | MATCH expr WITH literal COLON expr OR VAR COMMA VAR COLON expr { Match($2, $6, $8, $10, $12) }

printoperators:
  | PRINT term              { Print $2 }
  | PRINTNEWLINE term       { PrintNewLine $2}
  | PRINTSTRING QUOTE VAR QUOTE term    { PrintString ($3, $5)}

arithoperators:
  | INCR expr               { Increment ($2) }
  | DECR expr               { Decrement ($2) }
  | MINUS INT               { Val (-$2) }
  | expr PLUS expr	        { Plus ($1, $3) }
  | expr MINUS expr	        { Minus ($1, $3) }
  | expr TIMES expr	        { Times ($1, $3) }
  | expr DIVIDE expr	      { Divide ($1, $3) }

booleanoperators:
  | expr EQUAL expr         { Equal ($1, $3) }
  | expr LESS expr          { Less ($1, $3) }
  | expr GREATER expr       { Greater ($1, $3) }
  | expr AND expr           { And ($1, $3) }
  | expr OR expr            { Or ($1, $3) }
  | NOT expr                { Not $2 }

list:
  | elem                    { [$1] }
  | expr COMMA list         { $1 :: $3 }

elem:
  | expr                    { $1 }

literal:
    BOOL	 	                { Bool }
  | INTEGER         	      { Integer }
  | literal APPLY literal   { Apply ($1, $3) }
  | LBLOCK RBLOCK           { ListType [] }
  | LBLOCK listType RBLOCK  { ListType $2 }
  | LPAREN literal RPAREN   { $2 }

listType:
  | typeelem                { [$1] }
  | literal COMMA listType  { $1 :: $3 }

typeelem:
  | literal                 { $1 }

%%
