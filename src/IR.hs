module IR where

import AST (BinOp(..), UnOp(..), Type(..))

type Temp  = String
type Label = String

data Instr
  = MOVE Temp Temp                 -- t1 := t2
  | MOVEI Temp Int                 -- t := n
  | MOVEL Temp Label               -- t := label (para endereços de funções ou strings, se necessário)
  | OP BinOp Temp Temp Temp        -- t3 := t1 op t2
  | OPI BinOp Temp Temp Int        -- t2 := t1 op n
  | UNOP UnOp Temp                 -- t2 := unop t1
  | LABEL Label                    -- label
  | JUMP Label                     -- unconditional jump
  | COND BinOp Temp Temp Label Label -- conditional jump 
  | CALL Temp Label [Temp]         -- call a function
  | FUNC_BEGIN Label [Temp]        -- begin a function definition
  | FUNC_END Label                 -- end a function definition
  | RETURN Temp                    -- return from a function
  deriving (Eq, Show)







  