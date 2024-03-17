    module AST where

    type Identifier = String
    -- Data type for different types in the language
    data Type = TInt | TBool 
        deriving (Eq, Show)

    -- Function definition
    data Func = Funct Type String [Declaration] [Statement]
        deriving (Eq, Show)

    -- Declarations are for function parameters and variable declarations
    type Declaration = (Type, String)

    -- Statements include variable declarations, control structures, blocks, etc.
    data Statement =  VarDecl Type [String]
                    | Assign String Expression
                    | Declr Type String
                    | DeclAsgn Type String Expression
                    | If Expression Statement
                    | IfElse Expression Statement Statement
                    | While Expression Statement
                    | For (Maybe Statement) Expression Expression Statement
                    | Return Expression
                    | Block [Statement] 
                    | FunctionCall String [Expression]
                    deriving (Eq, Show)
                    
    -- Expressions in the language
    data Expression
        = Num Int
        | Var String
        | Str String
        | BoolLit Bool
        | Increment String
        | BinOp BinOp Expression Expression
        | UnOp UnOp Expression
        | FunctionCallExpr String [Expression]
        deriving (Eq, Show)

    -- Binary operations
    data BinOp = Add | Sub | Mul | Div | Mod
            | Eq | Neq | Lt | Lte | Gt | Gte
            | And | Or
            deriving (Eq, Show)

    -- Unary operations
    data UnOp = Not
        deriving (Eq, Show)

    -- data VarDecl = Assign String Expression
    --               | Declr Type String
    --               | DeclAsgn Type String Expression
    --               deriving Show