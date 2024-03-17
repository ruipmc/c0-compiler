{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module CodeGen where

import           AST
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State


-- symbol map identifiers to temporaries
type Table = Map Identifier Temp

-- the "supply" for generating temporaries and labels
-- counter for temporaries and labels
type Supply = (Int, Int)   

-- get a new temporary
newTemp :: State Supply Temp
newTemp 
  = do (temps,labels) <- State.get
       State.put (temps+1, labels)
       return ("t"++show temps)

-- get a new label
newLabel :: State Supply Label 
newLabel
  = do (temps,labels) <- State.get
       State.put (temps, labels+1)
       return ("L"++show labels)


-- get several temporaries
newTemps :: Int -> State Supply [Temp]
newTemps n | n > 0 = do
               t <- newTemp
               ts <- newTemps (n-1)
               return (t:ts)
           | otherwise = return []

---------------------------------------------------------------------------

-- translate an expression
transExpr :: Expression -> Table -> Temp -> State Supply [Instr]
transExpr (Var x) tabl dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "undefined variable"

transExpr (Num n) tabl dest 
  = return [MOVEI dest n]

transExpr (BinOp op e1 e2) tabl dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr e1 tabl temp1 
       code2 <- transExpr e2 tabl temp2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])

transExpr (FunctionCallExpr id args) tabl dest
  = do (code, temps) <- transArgs args tabl
       return (code ++ [CALL dest id temps])

transExpr (BoolLit b) _ dest = return [MOVEI dest (if b then 1 else 0)]

transExpr (UnOp op e) tabl dest = do
    temp <- newTemp
    code <- transExpr e tabl temp
    return (code ++ [UNOP op temp])  

-- translate functions arguments;
-- each one gets a new temporary
transArgs :: [Expression] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
      = do temp <- newTemp 
           code <- transExpr exp tabl temp 
           (code', temps') <- transArgs exps tabl
           return (code++code', temp:temps')

-- translate a statement
transStm :: Statement -> Table -> State Supply [Instr]
transStm (Assign var expr) tabl
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> transExpr expr tabl dest

-- Adicionando suporte para Declr
transStm (Declr typ var) tabl
  = do temp <- newTemp
       let tabl' = Map.insert var temp tabl
       return [] -- Aqui você pode optar por não gerar nenhuma instrução

-- Adicionando suporte para DeclAsgn
transStm (DeclAsgn typ var expr) tabl
  = do temp <- newTemp
       let tabl' = Map.insert var temp tabl
       exprCode <- transExpr expr tabl' temp
       return exprCode
                      
-- translate an if-then
transStm (If cond stm) tabl 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond cond tabl ltrue lfalse 
       code1  <- transStm stm tabl
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])

-- translate an if-then-else
transStm (IfElse cond stm1 stm2) tabl = do
       ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond cond tabl ltrue lfalse 
       code1 <- transStm stm1 tabl 
       code2 <- transStm stm2 tabl 
       return (code0 ++ [LABEL ltrue] ++ code1 ++ [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

-- translate a while statement
transStm (While cond stm) tabl =
  do lcond <- newLabel
     lbody <- newLabel
     lend <- newLabel
     code1 <- transCond cond tabl  lbody lend
     code2 <- transStm stm tabl 
     return ([LABEL lcond] ++ code1 ++ [LABEL lbody] ++ code2 ++ [JUMP lcond, LABEL lend])

-- translate the return statement
transStm (Return expr) tabl =
  do dest <- newTemp
     code <- transExpr expr tabl  dest
     return (code ++ [RETURN dest])

-- translate a block of statements
transStm (Block stms) tabl =
  transStmList stms tabl 

-- -- Translate 'For' Loops
-- transStm (For init cond incr body) tabl = do
--     lstart <- newLabel
--     lend <- newLabel
--     lbody <- newLabel
--     initCode <- maybe (return []) (`transStm` tabl) init
--     condCode <- transCond cond tabl lbody lend
--     incrCode <- (return []) (`transStm` tabl) incr
--     bodyCode <- transStm body tabl
--     return $ initCode ++ [LABEL lstart] ++ condCode ++
--              [LABEL lbody] ++ bodyCode ++ incrCode ++ [JUMP lstart, LABEL lend]

transCond :: Expression -> Table -> Label -> Label -> State Supply [Instr]
transCond (BinOp rel e1 e2) tabl ltrue lfalse 
  | rel == Lt || rel == Lte || rel == Eq =
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr e1 tabl temp1
         code2 <- transExpr e2 tabl temp2
         return ( code1 ++ code2 ++
                  [COND rel temp1 temp2 ltrue lfalse] )

-- translate a list of statements
transStmList :: [Statement] -> Table -> State Supply [Instr]
transStmList [] tabl = return []
transStmList (stm:rest) tabl = do
  code1 <- transStm stm tabl 
  code2 <- transStmList rest tabl
  return (code1 ++ code2)
  

transFun :: Func -> State Supply [Instr]
transFun (Funct _ name params body) = do
    let table = foldr (\(_, id) acc -> Map.insert id (id ++ "_temp") acc) Map.empty params 
    --Cria uma table que mapeia os identificadores dos parâmetros para temps
    bodyInstrs <- concat <$> mapM (`transStm` table) body
    let funcBeginInstr = FUNC_BEGIN name (map snd params)  
    return (funcBeginInstr : LABEL name : bodyInstrs ++ [FUNC_END name])