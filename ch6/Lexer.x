{

module Lexer where

}

%wrapper "posn"

$digit = [0-9]
$u     = [. \n]
@id    = [A-Za-z][0-9A-Za-z'_]*

tokens :-

$white+         ;
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ;

while     {(\p s -> While p)}
for       {(\p s -> For p)}
to        {(\p s -> To p)}
break     {(\p s -> Break p)}
let       {(\p s -> Let p)}
in        {(\p s -> In p)}
end       {(\p s -> End p)}
function  {(\p s -> Function p)}
var       {(\p s -> Var p)}
type      {(\p s -> Type p)}
array     {(\p s -> Array p)}
if        {(\p s -> If p)}
then      {(\p s -> Then p)}
else      {(\p s -> Else p)}
do        {(\p s -> Do p)}
of        {(\p s -> Of p)}
nil       {(\p s -> Nil p)}

","       {(\p s -> Comma p)}
":"       {(\p s -> Colon p)}
";"       {(\p s -> Semicolon p)}
"("       {(\p s -> LParen p)}
")"       {(\p s -> RParen p)}
"["       {(\p s -> LBracket p)}
"]"       {(\p s -> RBracket p)}
"{"       {(\p s -> LBrace p)}
"}"       {(\p s -> RBrace p)}
"."       {(\p s -> Dot p)}
"+"       {(\p s -> Plus p)}
"-"       {(\p s -> Minus p)}
"*"       {(\p s -> Multiply p)}
"/"       {(\p s -> Divide p)}
"="       {(\p s -> Equal p)}
"<>"      {(\p s -> NotEqual p)}
"<"       {(\p s -> LessThan p)}
"<="      {(\p s -> LessThanEqual p)}
">"       {(\p s -> GreaterThan p)}
">="      {(\p s -> GreaterThanEqual p)}
"&"       {(\p s -> And p)}
"|"       {(\p s -> Or p)}
":="      {(\p s -> Assign p)}

$digit+       {(\p s -> LInt (p, (read s :: Integer)))}
\"[^\"]*\"    {(\p (s:ss) -> LString (p, init ss))}
@id           {(\p s -> Id (p, s))}

{

data Token
  = While AlexPosn
  | For AlexPosn
  | To AlexPosn
  | Break AlexPosn
  | Let AlexPosn
  | In AlexPosn
  | End AlexPosn
  | Function AlexPosn
  | Var AlexPosn
  | Type AlexPosn
  | Array AlexPosn
  | If AlexPosn
  | Then AlexPosn
  | Else AlexPosn
  | Do AlexPosn
  | Of AlexPosn
  | Nil AlexPosn
  | Comma AlexPosn
  | Colon AlexPosn
  | Semicolon AlexPosn
  | LParen AlexPosn
  | RParen AlexPosn
  | LBracket AlexPosn
  | RBracket AlexPosn
  | LBrace AlexPosn
  | RBrace AlexPosn
  | Dot AlexPosn
  | Plus AlexPosn
  | Minus AlexPosn
  | Multiply AlexPosn
  | Divide AlexPosn
  | Equal AlexPosn
  | NotEqual AlexPosn
  | LessThan AlexPosn
  | LessThanEqual AlexPosn
  | GreaterThan AlexPosn
  | GreaterThanEqual AlexPosn
  | And AlexPosn
  | Or AlexPosn
  | Assign AlexPosn
  | Id (AlexPosn, String)
  | LInt (AlexPosn, Integer)
  | LString (AlexPosn, String)
  deriving (Eq, Show)

}
