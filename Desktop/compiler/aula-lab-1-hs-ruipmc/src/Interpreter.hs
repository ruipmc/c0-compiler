{-
  Exercícios para a Aula Laboratorial 1

  Escrever um interpretador em Haskell da sintaxe abstrata de
  programas sequenciais.

  Pedro Vasconcelos, 2022.

  Baseado num exercício do livro "Modern Compiler Implementation in
  ML", A. Appel.
-}

import Data.Maybe (fromMaybe)

module Interpreter where

--
-- sintaxe abstrata de programas sequenciais
--
type Ident = String  -- identificadores (nomes de variaveis)

data BinOp = Plus | Minus | Times | Div -- operações binárias
           deriving (Eq, Show)

data Stm = AssignStm Ident Exp   -- ident = exp
         | IncrStm Ident         -- ident++
         | CompoundStm Stm Stm   -- stm1; stm2
         deriving (Eq, Show)

data Exp = IdExp Ident           -- x, y, z ..
         | NumExp Int            -- 123
         | OpExp Exp BinOp Exp   -- e1+e2, e1*e2, ...
         deriving (Eq, Show)


{- Exercício 1.

Escrever duas funções recursivas para listar todos os identificadores
em comandos e expressões.

NOTA: escreva uma equação para cada construtor da sintaxe abstrata
acima. A função idsStm deve chamar idsExpr os comandos contêm sub-expressões.
-}

-- Função para listar identificadores em comandos (Stm)
idsStm :: Stm -> [Ident]
idsStm (AssignStm ident exp) = ident : idsExp exp  -- Adiciona o identificador e chama idsExp na expressão
idsStm (IncrStm ident)      = [ident]             -- Apenas o identificador
idsStm (CompoundStm stm1 stm2) = idsStm stm1 ++ idsStm stm2  -- Concatena os identificadores das duas sub-expressões

-- Função para listar identificadores em expressões (Exp)
idsExp :: Exp -> [Ident]
idsExp (IdExp ident)            = [ident]           -- Apenas o identificador
idsExp (NumExp _)               = []                -- Números não contêm identificadores
idsExp (OpExp exp1 _ exp2)      = idsExp exp1 ++ idsExp exp2  -- Concatena os identificadores das duas sub-expressões


-- NB: o que acontece se um identificador ocorrer mais do que uma vez?


{- Exercício 2: um interpretador funcional 

Escreva duas funções mutuamente recursivas para interpretar comandos
e expressões.

Represente tabelas associações de valores (inteiros) aos
identificadores como listas de pares.
Por exemplo, a lista [("x", 2), ("y", 0)] associa x -> 2, y -> 0.

Sugestões: use a função do prelúdio

lookup :: Eq a => a -> [(a,b)] -> Maybe b

para procurar o valor (se existir) associado a um identificador.
-}

type Table = [(Ident, Int)]    

        
-- Função auxiliar para procurar um valor associado a um identificador na tabela
lookupIdent :: Ident -> Table -> Maybe Int
lookupIdent ident table = lookup ident table

-- Função para interpretar comandos (Stm)
interpStm :: Stm -> Table -> Table
interpStm (AssignStm ident exp) table =
  let value = interpExp exp table
  in case lookupIdent ident table of
    Just _ -> (ident, value) : filter (\(id, _) -> id /= ident) table
    Nothing -> (ident, value) : table
interpStm (IncrStm ident) table =
  case lookupIdent ident table of
    Just value -> (ident, value + 1) : filter (\(id, _) -> id /= ident) table
    Nothing -> (ident, 1) : table
interpStm (CompoundStm stm1 stm2) table =
  let table' = interpStm stm1 table
  in interpStm stm2 table'

-- Função para interpretar expressões (Exp)
interpExp :: Exp -> Table -> Int
interpExp (IdExp ident) table =
  fromMaybe (error ("Identificador não encontrado: " ++ ident)) (lookupIdent ident table)
interpExp (NumExp n) _ = n
interpExp (OpExp exp1 op exp2) table =
  let value1 = interpExp exp1 table
      value2 = interpExp exp2 table
  in case op of
    Plus -> value1 + value2
    Minus -> value1 - value2
    Times -> value1 * value2
    Div -> value1 `div` value2


  
