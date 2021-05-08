%{
(* 補助的な変数、関数、型などの定義 *)
let make_fun vars expr =
  List.fold_right (fun v e -> Syntax.Fun (v, e)) vars expr
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE
%token EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <string> VAR
%token TRUE FALSE
%token IF THEN ELSE LET REC IN FUN ARROW
%token TRY WITH SHIFT RESET CONTROL PROMPT SHIFT0 RESET0 CONTROL0 PROMPT0
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ELSE IN ARROW WITH
%nonassoc EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
        { Syntax.Number ($1) }
| TRUE
        { Syntax.Bool (true) }
| FALSE
        { Syntax.Bool (false) }
| VAR
        { Syntax.Var ($1) }
| LPAREN expr RPAREN
        { $2 }

expr:
| simple_expr
        { $1 }
| expr PLUS expr
        { Syntax.Op ($1, Syntax.Plus, $3) }
| expr MINUS expr
        { Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
        { Syntax.Op ($1, Syntax.Times, $3) }
| expr DIVIDE expr
        { Syntax.Op ($1, Syntax.Divide, $3) }
| expr EQUAL expr
        { Syntax.Op ($1, Syntax.Equal, $3) }
| expr NOTEQUAL expr
        { Syntax.Op ($1, Syntax.NotEqual, $3) }
| expr LESS expr
        { Syntax.Op ($1, Syntax.Less, $3) }
| expr LESSEQUAL expr
        { Syntax.Op ($1, Syntax.LessEqual, $3) }
| expr GREATER expr
        { Syntax.Op ($3, Syntax.Less, $1) }
| expr GREATEREQUAL expr
        { Syntax.Op ($3, Syntax.LessEqual, $1) }
| MINUS expr %prec UNARY
        { Syntax.Op (Syntax.Number (0), Syntax.Minus, $2) }
| IF expr THEN expr ELSE expr
        { Syntax.If ($2, $4, $6) }
| LET VAR vars EQUAL expr IN expr
        { Syntax.Let ($2, make_fun $3 $5, $7) }
| LET REC VAR VAR vars EQUAL expr IN expr
        { Syntax.Letrec ($3, $4, make_fun $5 $7, $9) }
| FUN VAR vars ARROW expr
        { Syntax.Fun ($2, make_fun $3 $5) }
| TRY expr WITH expr
        { Syntax.Try ($2, $4) }
| SHIFT VAR ARROW expr
        { Syntax.S ($2, $4) }
| RESET simple_expr
        { Syntax.Angle_bracket ($2) }
| CONTROL VAR ARROW expr
        { Syntax.F ($2, $4) }
| PROMPT simple_expr
        { Syntax.Angle_bracket ($2) }
| SHIFT0 VAR ARROW expr
        { Syntax.S ($2, $4) }
| RESET0 simple_expr
        { Syntax.Angle_bracket0 ($2) }
| CONTROL0 VAR ARROW expr
        { Syntax.F ($2, $4) }
| PROMPT0 simple_expr
        { Syntax.Angle_bracket0 ($2) }
| app
        { $1 }

vars:
|
        { [] }
| VAR vars
        { $1 :: $2 }

app:
| simple_expr simple_expr
        { Syntax.App ($1, $2) }
| app simple_expr
        { Syntax.App ($1, $2) }
