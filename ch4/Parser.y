{
module Parser (parse) where
import Lexer
import Abs
}

%name parse
%tokentype { Token }
%error { parseError }

%token
while     { While $$ }
for       { For $$ }
to        { To _ }
break     { Break $$ }
let       { Let $$ }
in        { In _ }
end       { End _ }
function  { Function _ }
var       { Var _ }
type      { Type $$ }
array     { Array $$ }
if        { If $$ }
then      { Then _ }
else      { Else _ }
do        { Do _ }
of        { Of _ }
nil       { Nil _ }
','       { Comma _ }
':'       { Colon _ }
';'       { Semicolon _ }
'('       { LParen _ }
')'       { RParen _ }
'['       { LBracket $$ }
']'       { RBracket _ }
'{'       { LBrace _ }
'}'       { RBrace _ }
'.'       { Dot $$ }
'+'       { Plus $$ }
'-'       { Minus $$ }
'*'       { Multiply $$ }
'/'       { Divide $$ }
'='       { Equal $$ }
'<>'      { NotEqual $$ }
'<'       { LessThan $$ }
'<='      { LessThanEqual $$ }
'>'       { GreaterThan $$ }
'>='      { GreaterThanEqual $$ }
'&'       { And $$ }
'|'       { Or $$ }
':='      { Assign $$ }
STRING    { LString $$ }
INT       { LInt $$ }
ID        { Id $$ }

%right    of
%nonassoc do
%nonassoc else
%nonassoc ':='
%left     '&' '|'
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%%

program     : exp                                          { $1 }

decs        : tyDec decs                                   { $1 : $2 }
            | varDec decs                                  { $1 : $2 }
            | funDec decs                                  { $1 : $2 }
            | {- empty -}                                  { [] }

tyDec       : type ID '=' ty                               { typedec $2 $4 $1 }

ty          : ID                                           { namety $1 }
            | '{' tyFields '}'                             { RecordTy $2 }
            | array of ID                                  { arrayty $1 $3 }

tyFields    : fieldDec ',' tyFields                        { $1 : $3 }
            | fieldDec                                     { [ $1 ] }
            | {- empty -}                                  { [] }

fieldDec    : ID ':' ID                                    { field $1 $3 }

varDec      : var ID ':=' exp                              { vardec $2 $4 }
            | var ID ':' ID ':=' exp                       { vardecTy $2 $4 $6 }

funDec      : function ID '(' tyFields ')' '=' exp         { fundec $2 $4 $7 }
            | function ID '(' tyFields ')' ':' ID '=' exp  { fundecTy $2 $4 $7 $9 }

lValue      : ID                                           { simplevar $1 }
            | lValueB                                      { $1 }

lValueB     : ID '.' ID                                    { fieldvar (simplevar $1) $3 $2 }
            | lValueB '.' ID                               { fieldvar $1 $3 $2 }
            | ID '[' exp ']'                               { subscriptvar (simplevar $1) $3 $2 }
            | lValueB '[' exp ']'                          { subscriptvar $1 $3 $2 }

exp         : nil                                          { NilExp }
            | lValue                                       { VarExp $1 }
            | '(' seqExp ')'                               { SeqExp $2 }
            | INT                                          { intexp $1 }
            | STRING                                       { stringexp $1 }
            | '-' exp %prec UMINUS                         { opexp MinusOp zero $2 $1 }
            | ID '(' argList ')'                           { callexp $1 $3 }
            | infixExp                                     { $1 }
            | ID '{' recordField '}'                       { recordexp $1 $3 }
            | ID '[' exp ']' of exp                        { arrayexp $1 $3 $6 }
            | lValue ':=' exp                              { assignexp $1 $3 $2 }
            | if exp then exp else exp                     { ifexpThen $2 $4 $6 $1 }
            | if exp then exp %prec do                     { ifexp $2 $4 $1 }
            | while exp do exp                             { whileexp $2 $4 $1 }
            | for ID ':=' exp to exp do exp                { forexp $2 $4 $6 $8 $1 }
            | break                                        { breakexp $1 }
            | let decs in seqExp end                       { letexp $2 (SeqExp $4) $1 }

seqExp      : exp                                          { [ $1 ] }
            | seqExp ';' exp                               { $3 : $1 }
            | {- empty -}                                  { [] }

argList     : exp                                          { [ $1 ] }
            | argList ',' exp                              { $3 : $1 }
            | {- empty -}                                  { [] }

infixExp    : exp '*' exp                                  { opexp TimesOp $1 $3 $2  }
            | exp '/' exp                                  { opexp DivideOp $1 $3 $2 }
            | exp '+' exp                                  { opexp PlusOp $1 $3 $2 }
            | exp '-' exp                                  { opexp MinusOp $1 $3 $2 }
            | exp '=' exp                                  { opexp EqOp $1 $3 $2 }
            | exp '<>' exp                                 { opexp NeqOp $1 $3 $2 }
            | exp '>' exp                                  { opexp GtOp $1 $3 $2 }
            | exp '<' exp                                  { opexp LtOp $1 $3 $2 }
            | exp '>=' exp                                 { opexp GeOp $1 $3 $2 }
            | exp '<=' exp                                 { opexp LeOp $1 $3 $2 }
            | exp '&' exp                                  { ifexpThen $1 $3 zero $2 }
            | exp '|' exp                                  { ifexpThen $1 one $3 $2 }

recordField : ID '=' exp ',' recordField                   { recfield $1 $3 : $5 }
            | ID '=' exp                                   { [ recfield $1 $3 ] }
            | {- empty -}                                  { [] }


{

parseError :: [Token] -> a
parseError [] = error "Parse ERROR at: EOF"
parseError (x:xs) = error $ "Parse ERROR at: " ++ show x

zero = IntExp 0
one  = IntExp 1

simplevar    ((AlexPn _ l c), s) = SimpleVar s $ Pos l c
fieldvar     v (_, s) (AlexPn _ l c) = FieldVar v s $ Pos l c
subscriptvar v e (AlexPn _ l c) = SubscriptVar v e $ Pos l c

namety  ((AlexPn _ l c), s) = NameTy s $ Pos l c
arrayty (AlexPn _ l c) (_, s) = ArrayTy s $ Pos l c
field   ((AlexPn _ l c), f) (_, t) = Field f True t $ Pos l c

intexp    (_, i) = IntExp i
stringexp ((AlexPn _ l c), s) = StringExp s $ Pos l c
callexp   ((AlexPn _ l c), f) args = CallExp f args $ Pos l c
breakexp  (AlexPn _ l c) = BreakExp $ Pos l c
recfield  ((AlexPn _ l c), s) exp = (s, exp, Pos l c)
recordexp ((AlexPn _ l c), s) rs = RecordExp rs s $ Pos l c
assignexp var exp (AlexPn _ l c) = AssignExp var exp $ Pos l c
ifexpThen t thn e (AlexPn _ l c) = IfExp t thn (Just e) (Pos l c)
ifexp     t thn (AlexPn _ l c) = IfExp t thn Nothing (Pos l c)
whileexp  t b (AlexPn _ l c) = WhileExp t b $ Pos l c
forexp    (_, s) lo hi body (AlexPn _ l c) = ForExp s True lo hi body $ Pos l c
letexp    decs body (AlexPn _ l c) = LetExp decs body $ Pos l c
arrayexp  ((AlexPn _ l c), s) sz exp = ArrayExp s sz exp $ Pos l c

opexp op left right (AlexPn _ l c) = OpExp left op right $ Pos l c

typedec (_, s) t (AlexPn _ l c) = TypeDec [(s, t, Pos l c)]

vardecTy ((AlexPn _ l c), s) (_, ty) init = VarDec s True (Just ty) init $ Pos l c
vardec   ((AlexPn _ l c), s) init = VarDec s True Nothing init $ Pos l c

fundecTy ((AlexPn _ l c), s) params (_, ty) body = FunctionDec [FuncDec s params (Just ty) body $ Pos l c]
fundec   ((AlexPn _ l c), s) params body = FunctionDec [FuncDec s params Nothing body $ Pos l c]

}
