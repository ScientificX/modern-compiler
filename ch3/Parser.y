{
module Parser (parse) where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
while     { While _ }
for       { For _ }
to        { To _ }
break     { Break _ }
let       { Let _ }
in        { In _ }
end       { End _ }
function  { Function _ }
var       { Var _ }
type      { Type _ }
array     { Array _ }
if        { If _ }
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
'['       { LBracket _ }
']'       { RBracket _ }
'{'       { LBrace _ }
'}'       { RBrace _ }
'.'       { Dot _ }
'+'       { Plus _ }
'-'       { Minus _ }
'*'       { Multiply _ }
'/'       { Divide _ }
'='       { Equal _ }
'<>'      { NotEqual _ }
'<'       { LessThan _ }
'<='      { LessThanEqual _ }
'>'       { GreaterThan _ }
'>='      { GreaterThanEqual _ }
'&'       { And _ }
'|'       { Or _ }
':='      { Assign _ }
STRING    { LString _ $$ }
INT       { LInt _ $$ }
ID        { Id _ $$}

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

program     : exp                                          {}

decs        : tyDec decs                                   {}
            | varDec decs                                  {}
            | funDec decs                                  {}
            | {- empty -}                                  {}

tyDec       : type ID '=' ty                               {}

ty          : ID                                           {}
            | '{' tyFields '}'                             {}
            | array of ID                                  {}

tyFields    : fieldDec ',' tyFields                        {}
            | fieldDec                                     {}
            | {- empty -}                                  {}

fieldDec    : ID ':' ID                                    {}

varDec      : var ID ':=' exp                              {}
            | var ID ':' ID ':=' exp                       {}

funDec      : function ID '(' tyFields ')' '=' exp         {}
            | function ID '(' tyFields ')' ':' ID '=' exp  {}

lValue      : ID                                           {}
            | lValueB                                      {}

lValueB     : ID '.' ID                                    {}
            | lValueB '.' ID                               {}
            | ID '[' exp ']'                               {}
            | lValueB '[' exp ']'                          {}

exp         : nil                                          {}
            | lValue                                       {}
            | '(' seqExp ')'                               {}
            | INT                                          {}
            | STRING                                       {}
            | '-' exp %prec UMINUS                         {}
            | ID '(' argList ')'                           {}
            | infixExp                                     {}
            | ID '{' recordField '}'                       {}
            | ID '[' exp ']' of exp                        {}
            | lValue ':=' exp                              {}
            | if exp then exp else exp                     {}
            | if exp then exp %prec do                     {}
            | while exp do exp                             {}
            | for ID ':=' exp to exp do exp                {}
            | break                                        {}
            | let decs in seqExp end                       {}

seqExp      : exp                                          {}
            | exp ';' seqExp                               {}
            | {- empty -}                                  {}

argList     : exp                                          {}
            | argList ',' exp                              {}
            | {- empty -}                                  {}

infixExp    : exp '*' exp                                  {}
            | exp '/' exp                                  {}
            | exp '+' exp                                  {}
            | exp '-' exp                                  {}
            | exp '=' exp                                  {}
            | exp '<>' exp                                 {}
            | exp '>' exp                                  {}
            | exp '<' exp                                  {}
            | exp '>=' exp                                 {}
            | exp '<=' exp                                 {}
            | exp '&' exp                                  {}
            | exp '|' exp                                  {}

recordField : ID '=' exp ',' recordField                   {}
            | ID '=' exp                                   {}
            | {- empty -}                                  {}


{

parseError :: [Token] -> a
parseError [] = error "Parse ERROR at: EOF"
parseError (x:xs) = error $ "Parse ERROR at: " ++ show x

}
