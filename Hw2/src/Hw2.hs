-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map hiding (foldl, foldr, delete)
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Kaiwen Sun"
myEmail = "kas003@ucsd.edu"
mySID   = "A53091621"


-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:
{- Description: my understanding is that foldl applies a function f to the first
   element of a list, pass the result to the recursive call on the next element while
   in the next recursion the result serves as the base case. Continue until the recursion
   reaches the end of list. In other words, foldl accumulates the result based on the
   prefix of a list, and the accumulated result serves as the base case of next recursion.
   Finally, when we reach the end of the list, the base case is the accumulated result of
   the entire list. Return the accumulated result (level-by-level) to the top level of recursion.
 -}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b []     = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x:acc) [] xs

-- 3. Define `foldr` in terms of `foldl`:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = foldl (\b a -> f a b) b xs

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = foldr (\b a -> f a b) b xs

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?
{-
    Because Haskell is using lazy evaluation. The idea of it is that Haskell will
    not evaluate the expression until it's absolutely necessary. For example,
    foldl f z [1, 2, 3], it first creates a structure of unevaluated expressions
    (f (f (f z 1) 2) 3), when the list is traversed, it is necessary to evaluate
    those expression, Haskell will evaluate the inner most expression, and then
    the outer ones. With foldl', as soon as Haskell sees (f z 1), it evaluates
    it to a result, name it r1, and carry it to the next evaluation (f r1 2),
    and as soon as it sees (f r2 2), it evaluates to r3, and move forward to the
    next evaluation (f r3 3). Compared to the lazy version, there is no extra
    time to build unevaluated structure, so foldl' is faster.
-}

-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

-- Define a `delete` function for BSTs of this type:

-- Finds inorder successor of BST k v and returns a tuple of key and value
inorderSucc :: (Ord k) => BST k v -> (k, v)
inorderSucc (Bind k v _ Emp) = (k, v)
inorderSucc (Bind _ _ _ r)   = inorderSucc r

delete :: (Ord k) => k -> BST k v -> BST k v
delete k (Bind key val lt rt)
    | k <  key = Bind key val (delete k lt) rt
    | k >  key = Bind key val lt (delete k rt)
    | k == key = case lt of
                 Emp -> rt
                 _   -> Bind keySucc valSucc (delete keySucc lt) rt where
                     (keySucc, valSucc) = inorderSucc lt
    | otherwise = error "Something is wrong."

-- Part 3: An Interpreter for WHILE
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as

type Variable = String

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value`

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return
-- the value `0`. In future assignments, we will add this as a
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State`
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function

evalE :: Expression -> State Store Value

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract
-- the value of the "current store" in a variable `s` use `s <- get`.

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus (IntVal i) (IntVal j) = IntVal (i+j)
evalOp Minus (IntVal i) (IntVal j) = IntVal (i-j)
evalOp Times (IntVal i) (IntVal j) = IntVal (i*j)
evalOp Divide (IntVal i) (IntVal j) = IntVal (i `div` j)
evalOp Gt (IntVal i) (IntVal j) = BoolVal (i>j)
evalOp Ge (IntVal i) (IntVal j) = BoolVal (i>=j)
evalOp Lt (IntVal i) (IntVal j) = BoolVal (i<j)
evalOp Le (IntVal i) (IntVal j) = BoolVal (i<=j)

-- >

evalE (Var x)      = do
                        nvMap <- get
                        return (findWithDefault (IntVal 0) x nvMap)

evalE (Val v)      = return v
evalE (Op o e1 e2) = do
                        (IntVal v1) <- evalE e1
                        (IntVal v2) <- evalE e2
                        return (evalOp o (IntVal v1) (IntVal v2))

-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.

evalS (Assign x e )    = do
                            nvMap <- get
                            v <- evalE e
                            -- changedMap <- insert x v nvMap
                            put (insert x v nvMap)

evalS w@(While e s)    = do
                            v <- evalE e
                            case v of
                                BoolVal True -> do
                                                evalS s
                                                evalS w
                                BoolVal False -> return ()
                                IntVal _ -> return ()
evalS Skip             = return ()
evalS (Sequence s1 s2) = do
                         evalS s1
                         evalS s2
evalS (If e s1 s2)     = do
                         v <- evalE e
                         case v of
                             BoolVal True -> evalS s1
                             BoolVal False -> evalS s2
                             IntVal _ -> return ()

-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function

execS :: Statement -> Store -> Store
execS stmt = execState (evalS stmt)

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`.
-- **Hint:** You may want to use the library function

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will
-- "run" a statement starting with the `empty` store (where no
-- variable is initialized). Running the program should print
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP -- intP is tried first, if intP failed and no input comsumed, boolP is tried

-- To do so, fill in the implementations of

intP :: Parser Value
intP = do
       charArray <- many1 digit
       return (IntVal (read charArray::Int))

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
             k <- string s -- potential error
             return x

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

-- Continue to use the above to parse the binary operators

opP :: Parser Bop
opP = constP "+"  Plus   <|>
      constP "-"  Minus  <|>
      constP "*"  Times  <|>
      constP "/"  Divide <|>
      constP ">"  Gt     <|>
      constP ">=" Ge     <|>
      constP "<"  Lt     <|>
      constP "<=" Le

-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values

exprP :: Parser Expression
exprP = (try expOpExp) <|> (try enclosedExp) <|> (try variable) <|> (try value)
 where
    value = do
            someVal <- valueP
            return (Val someVal)
    variable = do
               someVar <- varP
               return (Var someVar)
    enclosedExp = do
                  string "("
                  someExp <- exprP
                  string ")"
                  return someExp
    expOpExp = do
               someExp1 <- (try variable) <|> (try value) <|> (try enclosedExp)
               skipMany space
               someOp <- opP
               skipMany space
               someExp2 <- exprP
               return (Op someOp someExp1 someExp2)

-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser Statement
statementP = (try seqStmt) <|> (try asnStmt) <|> (try ifStmt) <|> (try whileStmt) <|> (try skipStmt)
    where
         seqStmt = do
                  skipMany space
                  someStmt1 <- try asnStmt <|> ifStmt <|> whileStmt <|> skipStmt
                  string ";"
                  skipMany space
                  someStmt2 <- statementP
                  skipMany space
                  return (Sequence someStmt1 someStmt2)
         asnStmt = do
                   skipMany space
                   someVar <- varP
                   skipMany space
                   string ":="
                   skipMany space
                   someExpr <- exprP
                   skipMany space
                   return (Assign someVar someExpr) -- ok
         ifStmt = do
                  skipMany space
                  string "if"
                  skipMany space
                  someExpr <- exprP
                  skipMany space
                  string "then"
                  skipMany space
                  someStmt1 <- statementP
                  skipMany space
                  string "else" -- else is mandatory, makes it easier to parse
                  skipMany space
                  someStmt2 <- statementP
                  skipMany space
                  string "endif"
                  return (If someExpr someStmt1 someStmt2) -- ok
         whileStmt = do
                     skipMany space
                     string "while"
                     skipMany space
                     condition <- exprP
                     skipMany space
                     string "do"
                     skipMany space
                     someStmt <- statementP
                     skipMany space
                     string "endwhile"
                     return (While condition someStmt) -- ok
         skipStmt = do
                    skipMany space
                    string "skip"
                    skipMany space
                    return Skip

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~
