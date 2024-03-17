{
module Parser where
import Lexer
import AST

}

%name parser
%tokentype { Token }
%error { parseError }

%token

-- Keywords
"int" { Token_Int }
"bool" { Token_Bool }
"true" { Token_True }
"false" { Token_False }
"if" { Token_If }
"else" { Token_Else }
"while" { Token_While }
"for" { Token_For }
"return" { Token_Return }
"break" { Token_Break }
"continue" { Token_Continue }

-- Operators
"+" { Token_Plus }
"-" { Token_Minus }
"*" { Token_Times }
"/" { Token_Divide }
"%" { Token_Modulo }
"==" { Token_EqualEqual }
"!=" { Token_NotEqual }
"<" { Token_Less }
"<=" { Token_LessEqual }
">" { Token_Greater }
">=" { Token_GreaterEqual }
"=" { Token_Assign }
"!" { Token_Not }
"&&" { Token_AndAnd }
"||" { Token_OrOr }

-- Punctuation
"{" { Token_LBrace }
"}" { Token_RBrace }
"(" { Token_LParenth }
")" { Token_RParenth }
";" { Token_Semicolon }
"," { Token_Comma }
"++" { Token_Increment }

-- Identifiers, numbers, and strings
numeral { Token_Integer $$ }
string { Token_String $$ }
identifier { Token_Identifier $$ }

%left "||"
%left "&&"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%left "!" 
%left "if" "else" 

%%
--GRAMMAR---

Funcs
    : Func { [$1] }
    | Funcs Func { $1 ++ [$2] }

Func : Type identifier "(" Declaration ")" "{" StatementList "}" { Funct $1 $2 $4 $7 }

StatementList
    : Statement { [$1] }
    | Statement StatementList { $1 : $2 }

Statement
    : VarDeclaration                                   { $1}    
    | identifier "=" Expression ";"                   { Assign $1 $3}
    | Type identifier ";"                              { Declr $1 $2 }
    | Type identifier "=" Expression ";"                 { DeclAsgn $1 $2 $4 }
    | "if" "(" Expression ")" Statement                   { If $3 $5 }
    | "if" "(" Expression ")" Statement "else" Statement  { IfElse $3 $5 $7 }
    | "while" "(" Expression ")" Statement                  { While $3 $5 } 
    | "for" "(" ForInit ";" Expression ";" Expression")" Statement { For $3 $5 $7 $9 }
    | identifier "(" ExprList ")" ";"                 {FunctionCall $1 $3}
    | "return" Expression ";"                           { Return $2 }
    | "{" StatementList "}"                                 { Block $2 }

VarDeclaration
    : Type IdentifierList ";" { VarDecl $1 $2 }

IdentifierList
    : identifier { [$1] }
    | identifier "," IdentifierList { $1 : $3 }

ForInit
    : {- empty -}                                   { Nothing }
    | identifier "=" Expression                     { Just (Assign $1 $3) }

-- VarDecl : identifier "=" Expression ";"                   { Assign $1 $3}
--       | Type identifier ";"                              { Declr $1 $2 }
--       | Type identifier "=" Expression ";"                 { DeclAsgn $1 $2 $4 } -- declaration and assignment

Declaration: {- empty -} {[]}
    | Type identifier  {[($1, $2)]}
    | Declaration "," Type identifier  {$1 ++ [($3, $4)]}

Expression
    : numeral                                   { Num $1 }
    | identifier                                { Var $1 }
    | string                                    { Str $1 }
    | identifier "(" ExprList ")"               { FunctionCallExpr $1 $3 }
    | "true"                                    { BoolLit True }
    | "false"                                   { BoolLit False }
    | identifier "++"                           { Increment $1 }
    | Expression "+" Expression                 { BinOp Add $1 $3 }
    | Expression "-" Expression                 { BinOp Sub $1 $3 }
    | Expression "*" Expression                 { BinOp Mul $1 $3 }
    | Expression "/" Expression                 { BinOp Div $1 $3 }
    | Expression "%" Expression                 { BinOp Mod $1 $3 }
    | Expression "==" Expression                { BinOp Eq $1 $3 }
    | Expression "!=" Expression                { BinOp Neq $1 $3 }
    | Expression "<" Expression                 { BinOp Lt $1 $3 }
    | Expression "<=" Expression                { BinOp Lte $1 $3 }
    | Expression ">" Expression                 { BinOp Gt $1 $3 }
    | Expression ">=" Expression                { BinOp Gte $1 $3 }
    | Expression "&&" Expression                { BinOp And $1 $3 }
    | Expression "||" Expression                { BinOp Or $1 $3 }
    | "!" Expression                            { UnOp Not $2 }

ExprList
    : ExprList "," Expression                    { $1 ++ [$3] }
    | Expression                                { [$1] }
    | {-empty-}                                   { [] }

Type
    : "int"                                     { TInt }
    | "bool"                                    { TBool }

{
parseError :: [Token] -> a
parseError toks = error $ "Parse error at: " ++ (unlines . map show . take 10) toks
}