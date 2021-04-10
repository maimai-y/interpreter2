%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
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
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
|	MINUS expr %prec UNARY
	{ Syntax.Op (Syntax.Number (0), Syntax.Minus, $2) }
| expr PLUS expr
	{ Syntax.Op ($1, Syntax.Plus, $3) }
| expr MINUS expr
	{ Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Syntax.Times, $3) }
| expr DIVIDE expr
	{ Syntax.Op ($1, Syntax.Divide, $3) }
