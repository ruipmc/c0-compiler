{
  module Lexer where
}

%wrapper "basic"

-- Regex Expressions
-- $white = [\ \t\n\r\"\\]
$white = [\ \t\n\r\v\f]
$digit = [0-9]
$alpha = [a-zA-Z]


tokens :-

$white+ ; -- ignore white chars

-- Keywords
-- main { \_ -> Token_Main}
int { \_ -> Token_Int }
bool { \_ -> Token_Bool }
true { \_ -> Token_True }
false { \_ -> Token_False }
if { \_ -> Token_If }
else { \_ -> Token_Else }
while { \_ -> Token_While }
var { \_ -> Token_Var }
return { \_ -> Token_Return }
-- scan_int { \_ -> Token_ScanInt }
-- print_int { \_ -> Token_PrintInt }
-- print_str { \_ -> Token_PrintStr }
for { \_ -> Token_For }
break { \_ -> Token_Break }
continue { \_ -> Token_Continue }

-- Operators
"+" { \_ -> Token_Plus }
"-" { \_ -> Token_Minus }
"*" { \_ -> Token_Times }
"/" { \_ -> Token_Divide }
"%" { \_ -> Token_Modulo }
"==" { \_ -> Token_EqualEqual }
"!=" { \_ -> Token_NotEqual }
"<" { \_ -> Token_Less }
"<=" { \_ -> Token_LessEqual }
">" { \_ -> Token_Greater }
">=" { \_ -> Token_GreaterEqual }
"=" { \_ -> Token_Assign }
"!" { \_ -> Token_Not }
"&&" { \_ -> Token_AndAnd }
"||" { \_ -> Token_OrOr }

-- Punctuation
"{" { \_ -> Token_LBrace }
"}" { \_ -> Token_RBrace }
"(" { \_ -> Token_LParenth }
")" { \_ -> Token_RParenth }
";" { \_ -> Token_Semicolon }
"," { \_ -> Token_Comma }
"++" { \_ -> Token_Increment }

-- Comments
"//".* ;
"/*"([^\*] | [\n] | ("*"+([^\*\/] | [\n] )))*"*"+"/";


-- Identifiers, integers and strings
$digit+ { \s -> Token_Integer (read s) }
$alpha($alpha|$digit|_)* { \s -> Token_Identifier s }
\"[^\"\n]*\" { \s -> Token_String (tail(init s)) }


{
data Token
  = Token_Int
  | Token_Bool
  -- | Token_Main
  | Token_True
  | Token_False
  | Token_If
  | Token_Else
  | Token_While
  | Token_Var
  | Token_Return
  -- | Token_ScanInt
  -- | Token_PrintInt
  -- | Token_PrintStr
  | Token_For
  | Token_Break
  | Token_Continue
  | Token_Plus
  | Token_Minus
  | Token_Times
  | Token_Divide
  | Token_Modulo  
  | Token_EqualEqual
  | Token_NotEqual
  | Token_Less
  | Token_LessEqual
  | Token_Greater
  | Token_GreaterEqual
  | Token_Assign
  | Token_Not
  | Token_AndAnd
  | Token_OrOr
  | Token_LBrace
  | Token_RBrace
  | Token_LParenth
  | Token_RParenth
  | Token_Semicolon
  | Token_Comma
  | Token_Increment
  | Token_String String
  | Token_Identifier String
  | Token_Integer Int
  deriving (Eq, Show)
}

