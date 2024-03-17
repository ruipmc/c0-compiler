-- module Main where

-- import Lexer
-- import Parser
-- import AST

-- main = do
--   txt <- getContents
--   print (parser $ alexScanTokens txt)


module Main where

import AST
import IR
-- choose one version of the code gen module
import CodeGen
-- import CodeGen3 
-- NB: version 1 doesn't use the same API; try it on the REPL
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
  
  -- starting values for temporary and label counters
initialSupply :: Supply
initialSupply = (0, 0)

-- run a code generation action with initial supply
runCodeGen :: State Supply [Instr] -> [Instr]
runCodeGen gen = State.evalState gen Main.initialSupply

main :: IO ()
main = do
  -- Example 1
  print example1
  printIR (runCodeGen (do dest <- newTemp
                          transExpr example1 Map.empty dest))
  
  -- Example 2
  line  
  print example2
  printIR (runCodeGen (do tx <- newTemp
                          ty <- newTemp
                          let tabl = Map.fromList [("x", tx), ("y", ty)]
                          transStm example2 tabl ))
  -- Example 3
  line  
  print example3
  printIR (runCodeGen (do ta <- newTemp
                          tb <- newTemp
                          tr <- newTemp
                          let tabl = Map.fromList [("a", ta), ("b", tb), ("r",tr)]
                          transStm example3 tabl ))
  -- Example 4
  line
  print example4
  printIR (runCodeGen (do result <- newTemp
                          transExpr example4 Map.empty result))
  

-- print an horizontal line
line :: IO () 
line = putStrLn (replicate 40 '-')

-- print a list of IR instructions
printIR :: [Instr] -> IO ()
printIR = mapM_ print

example1 :: Expression
example1 = BinOp Add (BinOp Mul (Num 2) (Num 3)) (Num 1)

example2 :: Statement
example2 = IfElse (BinOp Lt (Var "x") (Num 0)) (Assign "y" (Num 1)) (Assign "y" (Num 2))

example3 :: Statement
example3 = While (BinOp Lt (Num 0) (Var "b")) 
            (Block [ Assign "r" (BinOp Mod (Var "a") (Var "b")), 
                     Assign "a" (Var "b"), 
                     Assign "b" (Var "r") ])

example4 :: Expression
example4 = BinOp Add 
            (FunctionCallExpr "f" [BinOp Sub (Num 1) (Num 2)]) 
            (FunctionCallExpr "g" [BinOp Mul (Num 3) (Num 4)])

example5 :: Func
example5 = Funct TInt "max3" [(TInt, "x"), (TInt, "y"), (TInt, "z")] 
           [ Assign "m" (Var "x"), 
             IfElse (BinOp Lt (Var "x") (Var "y")) (Assign "m" (Var "y")) (Assign "m" (Var "x")), 
             IfElse (BinOp Lt (Var "m") (Var "z")) (Assign "m" (Var "z")) (Assign "m" (Var "m")), 
             Return (Var "m") ]

-- example6 :: Func
-- example6 = Funct TInt "main" [[] [VarDecl TInt ["s","n"],Assign "s" (Num 0),Assign "n" (Num 1),While (BinOp Lte (Var "n") (Num 10)) (Block [Assign "s" (BinOp Add (Var "s") (BinOp Mul (Var "n") (Var "n"))),Assign "n" (BinOp Add (Var "n") (Num 1))]),FunctionCall "print_int" [Var "n"]]]


-- -- examples ----------------------------------------------------------------
-- example1 :: Exprx\
-- example1
--   = Op Plus (Op Mult (Num 2) (Num 3)) (Num 1)  

-- example2 :: Statement
-- example2
--   = IfElse
--     (Op Lt (Var "x") (Num 0))
--     (Assign "y" (Num 1))
--     (Assign "y" (Num 2))

-- example3 :: Statement
-- example3
--   = While (Op Lt (Num 0) (Var "b"))
--     ( Block [ Assign "r" (Op Mod (Var "a") (Var "b"))
--             , Assign "a" (Var "b")
--             , Assign "b" (Var "r")
--             ]
--     )

-- example4 :: Expression
-- example4
--   = Op Plus
--     (Fun "f" [Op Minus (Num 1) (Num 2)])
--     (Fun "g" [Op Mult (Num 3) (Num 4)])


-- example5 :: Func
-- example5 = FunDef "max3" ["x","y","z"] ["m"]
--            [ Assign "m" (Var "x")
--            , IfElse (Op Lt (Var "x") (Var "y"))
--              (Assign "m" (Var "y"))
--              (Assign "m" (Var "x"))
--            , IfElse (Op Lt (Var "m") (Var "z"))
--              (Assign "m" (Var "z"))
--              (Assign "m" (Var "m"))
--            , Return (Var "m")
--            ]