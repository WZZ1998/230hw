-- ---
-- title: Homework #3, Due Monday, Feb 29, 2016 (23:59:59 PST)
-- ---

-- Preliminaries
-- =============

-- To complete this homework,

-- 1. download [Hw3.tar.gz](../static/Hw2.tgz),
-- 2. unzip it by `tar -zxvf Hw3.tgz`
-- 3. `cd Hw3` ,
-- 4. Fill in each `error "TODO"` in `src/Hw3.hs`
-- 5. Submit by mailing the completed `Hw3.hs` to `cse230@goto.ucsd.edu` with the
--    subject "HW3".

-- You will receive a confirmation email after submitting.

-- Your code *must* typecheck against the given type signatures.
-- Feel free to add your own tests to this file to exercise the
-- functions you write. As before, you can compile the code by
-- doing `stack build` and load in `ghci` by doing `stack ghci`.

-- **Learn to read the [documentation](http://hackage.haskell.org)**

{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveGeneric             #-}

module Hw3 where

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import GHC.Generics
import Test.QuickCheck hiding ((===))
import Control.Monad (forM, forM_)
import Data.List (transpose, intercalate)


quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n}

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Kaiwen Sun"
myEmail = "kas003@ucsd.edu"
mySID   = "A53091621"



-- Problem 1: An Interpreter for WHILE++
-- =====================================

-- Previously, you wrote a simple interpreter for *WHILE*.
-- For this problem, you will use monad transformers to build
-- an evaluator for *WHILE++* which, adds exceptions and I/O
-- to the original language.

-- As before, we have variables, and expressions.

type Variable = String
type Store    = Map.Map Variable Value
-- >
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Generic)
-- >
instance Error Value
-- >
data Expression =
    Var Variable
  | Val Value
  | Op  Bop Expression Expression
  deriving (Show)
-- >
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show)

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Sequence Statement Statement
  | Skip
  | Print String Expression          -- new
  | Throw Expression                 -- new
  | Try Statement Variable Statement -- new
  deriving (Show)

-- The only new constructs are the `Print`, `Throw` and the `Try` statements.

-- - `Print s e` should print out (eg to stdout) log the string corresponding
--   to the string `s` followed by whatever `e` evaluates to, followed by a
--   newline --- for example, `Print "Three: " (IntVal 3)' should display
--   "Three: IntVal 3\n",

-- - `Throw e` evaluates the expression `e` and throws it as an exception, and

-- - `Try s x h` executes the statement `s` and if in the course of
--   execution, an exception is thrown, then the exception comes shooting
--   up and is assigned to the variable `x` after which the *handler*
--   statement `h` is executed.

-- We will use the `State` [monad][2] to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Write a function

-- runtest1 = execute Map.empty testprog1
-- runtest2 = execute Map.empty testprog2

evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m () -- () <=> unit/void
evalS (Assign k e)      = do nvMap <- get
                             v <- evalE e
                             put (Map.insert k v nvMap)

evalS w@(While e s)     = do v <- evalE e
                             case v of
                                 BoolVal True -> do evalS s
                                                    evalS w
                                 BoolVal False -> return ()
                                 IntVal _ -> return ()

evalS Skip              = return ()

evalS (Print str e)     = do val <- evalE e
                             tell (str ++ show val ++ "\n")

                       -- throwError :: e -> m a
evalS (Throw e)         = do val <- evalE e
                             throwError val

                       -- catchError :: m a -> (e -> m a) -> m a
evalS (Try s k h)       = catchError (evalS s)
                          (\exn -> do nvMap <- get
                                      put (Map.insert k exn nvMap)
                                      evalS h)

evalS (Sequence s1 s2)  = do evalS s1
                             evalS s2

evalS (If e s1 s2)      = do v <- evalE e
                             case v of
                                 BoolVal True  -> evalS s1
                                 BoolVal False -> evalS s2
                                 IntVal _      -> return ()

evalOp :: Bop -> Value -> Value -> Value -- old code copied from hw2
evalOp Plus (IntVal i) (IntVal j) = IntVal (i+j)
evalOp Minus (IntVal i) (IntVal j) = IntVal (i-j)
evalOp Times (IntVal i) (IntVal j) = IntVal (i*j)
evalOp Divide (IntVal i) (IntVal j) = IntVal (i `div` j)
evalOp Gt (IntVal i) (IntVal j) = BoolVal (i>j)
evalOp Ge (IntVal i) (IntVal j) = BoolVal (i>=j)
evalOp Lt (IntVal i) (IntVal j) = BoolVal (i<j)
evalOp Le (IntVal i) (IntVal j) = BoolVal (i<=j)

evalE :: (MonadState Store m, MonadError Value m) => Expression -> m Value
evalE (Val x) = return x
evalE (Var k) = do nvMap <- get
                   case Map.lookup k nvMap of
                       Just v  -> return v
                       Nothing -> throwError (IntVal 0)

evalE (Op op e1 e2) = do val1 <- evalE e1
                         val2 <- evalE e2
                         case (val1, op, val2) of
                             (BoolVal _, _, _) -> throwError (IntVal 2)
                             (_, _, BoolVal _) -> throwError (IntVal 2)
                             (_, Divide, IntVal 0)    -> throwError (IntVal 1)
                             (val1, op, val2)  -> return (evalOp op val1 val2)

-- Next, we will implement a *concrete instance* of a monad `m` that
-- satisfies the above conditions, by filling in a suitable definition:

type Eval a = ErrorT Value (WriterT String (State Store)) a  -- EWS

-- Now, we implement a function to *run* the action from a given store:

runEval :: Eval a -> Store -> ((Either Value a, String), Store)
runEval act sto = runState (runWriterT (runErrorT act)) sto

-- When you are done, you will get an implementation:

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute sto stmt   = (sto', leftMaybe v, l)
  where
    ((v, l), sto') = runEval (evalS stmt) sto

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing

-- such that `execute st s` returns a triple `(st', exn, log)` where

-- - `st'` is the output state,
-- - `exn` is possibly an exception (if the program terminates with an uncaught exception),
-- - `log` is the log of messages generated by the `Print` statements.

-- Requirements
-- ------------

-- In the case of exceptional termination, the `st'` should be the state *at
-- the point where the last exception was thrown, and `log` should include all
-- the messages *upto* that point -- make sure you stack (*order*) your transformers
-- appropriately!

-- - Reading an undefined variable should raise an exception carrying the value `IntVal 0`.

-- - Division by zero should raise an exception carrying the value `IntVal 1`.

-- - A run-time type error (addition of an integer to a boolean, comparison of
--   two values of different types) should raise an exception carrying the value
--   `IntVal 2`.

-- Example 1
-- ---------

-- If `st` is the empty state (all variables undefined) and `s` is the program

-- ~~~~~{.haskell}
-- X := 0 ;
-- Y := 1 ;
-- print "hello world: " X;
-- if X < Y then
--   throw (X+Y)
-- else
--   skip
-- endif;
-- Z := 3
-- ~~~~~

-- then `execute st s` should return the triple

-- ~~~~~{.haskell}
-- (fromList [("X", IntVal 0), ("Y",  IntVal 1)], Just (IntVal 1), "hello world: IntVal 0\n")
-- ~~~~~

-- The program is provided as a Haskell value below:

mksequence = foldr Sequence Skip

testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Print "hello world: " $ Var "X",
                        If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
                                                       Skip,
                        Assign "Z" $ Val $ IntVal 3]

-- Example 2
-- ---------

-- If `st` is the empty state (all variables undefined) and `s` is the program

-- ~~~~~{.haskell}
-- X := 0 ;
-- Y := 1 ;
-- try
--   if X < Y then
--     A := 100;
--     throw (X+Y);
--     B := 200
--   else
--     skip
--   endif;
-- catch E with
--   Z := E + A
-- endwith
-- ~~~~~

-- then `execute st s` should return the triple

-- ~~~~~{.haskell}
-- ( fromList [("A", IntVal 100), ("E", IntVal 1)
--            ,("X", IntVal 0), ("Y", IntVal 1)
--  	   ,("Z", IntVal 101)]
-- , Nothing
-- , "")
-- ~~~~~

-- Again, the program as a Haskell value:

testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Try (If (Op Lt (Var "X") (Var "Y"))
                                (mksequence [Assign "A" $ Val $ IntVal 100,
                                             Throw (Op Plus (Var "X") (Var "Y")),
                                             Assign "B" $ Val $ IntVal 200])
                                Skip)
                            "E"
                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]


-- Problem 2: Binary Search Trees Revisited
-- ========================================

-- Recall the old type of binary search trees from
-- [HW2](/homeworks/Hw2.html).

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)
-- >
toBinds ::  BST t t1 -> [(t, t1)]
toBinds Emp            = []
toBinds (Bind k v l r) = toBinds l ++ [(k,v)] ++ toBinds r

-- The following function tests whether a tree satisfies the
-- binary-search-order invariant.

isBSO ::  Ord a => BST a b -> Bool
isBSO Emp            = True
isBSO (Bind k v l r) = all (< k) lks && all (k <) rks && isBSO l && isBSO r
  where lks = map fst $ toBinds l
        rks = map fst $ toBinds r

-- Finally, to test your implementation, we will define a
-- type of operations over trees

data BSTop k v = BSTadd k v | BSTdel k
                 deriving (Eq, Show)

-- and a function that constructs a tree from a sequence of operations

ofBSTops ::  Ord k => [BSTop k v] -> BST k v
ofBSTops    = foldr doOp Emp
  where doOp (BSTadd k v) = bstInsert k v
        doOp (BSTdel k)   = bstDelete k

-- and that constructs a reference `Map` from a sequence of operations

mapOfBSTops ::  Ord k => [BSTop k a] -> Map.Map k a
mapOfBSTops = foldr doOp Map.empty
  where doOp (BSTadd k v) = Map.insert k v
        doOp (BSTdel k)   = Map.delete k

-- and functions that generate an arbitrary BST operations

keys :: [Int]
keys = [0..10]
-- >
genBSTadd, genBSTdel, genBSTop ::  Gen (BSTop Int Char)
genBSTadd = liftM2 BSTadd (elements keys) (elements ['a'..'z'])
genBSTdel = liftM BSTdel (elements keys)
genBSTop  = frequency [(5, genBSTadd), (1, genBSTdel)]

-- (a) Insertion
-- -------------

-- Write an insertion function
-- to run test, just run: quickCheck prop

bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
bstInsert k v Emp = Bind k v Emp Emp
bstInsert k v (Bind k' v' left right)
    | k == k' = Bind k' v left right
    | k <  k' = insBalance k (Bind k' v' (bstInsert k v left) right)
    | k >  k' = insBalance k (Bind k' v' left (bstInsert k v right))

-- such that `bstInsert k v t` inserts a key `k` with value
-- `v` into the tree `t`. If `k` already exists in the input
-- tree, then its value should be *replaced* with `v`. When you
-- are done, your code should satisfy the following QC properties.

prop_insert_bso :: Property
prop_insert_bso = forAll (listOf genBSTadd) $ \ops ->
                    isBSO (ofBSTops ops)
-- >
prop_insert_map = forAll (listOf genBSTadd) $ \ops ->
                    toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)

-- (b) Deletion
-- ------------

-- Write a deletion function for BSTs of this type:

-- five cases
--   totally empty tree
--   both branches are empty
--   left branch is empty
--   right branch is empty
--   both branch are not empty
bstDelete :: (Ord k) => k -> BST k v -> BST k v
bstDelete _ Emp = Emp
bstDelete k' (Bind k v Emp Emp)
    | k' == k   = Emp
    | otherwise = Bind k v Emp Emp
bstDelete k' (Bind k v Emp right)
    | k' == k = right
    | k' >  k = delBalance (Bind k v Emp (bstDelete k' right))
    | k' <  k = Bind k v Emp right
bstDelete k' (Bind k v left Emp)
    | k' == k = left
    | k' >  k = Bind k v left Emp
    | k' <  k = delBalance (Bind k v (bstDelete k' left) Emp)
bstDelete k' (Bind k v left right)
    | k' >  k = delBalance (Bind k v left (bstDelete k' right))
    | k' <  k = delBalance (Bind k v (bstDelete k' left) right)
    | k' == k = Bind k'' v'' left right' where
        (k'',v'') = findMinNode right
        right' = bstDelete k'' right

findMinNode :: BST k v -> (k,v)
findMinNode Emp = error "The root is empty!"
findMinNode (Bind k v Emp _) = (k,v)
findMinNode (Bind k v left _) = findMinNode left


-- such that `bstDelete k t` removes the key `k` from the tree `t`.
-- If `k` is absent from the input tree, then the tree is returned
-- unchanged as the output. When you are done, your code should
-- satisfy the following QC properties.

prop_delete_bso :: Property
prop_delete_bso = forAll (listOf genBSTop) $ \ops ->
                    isBSO (ofBSTops ops)
-- >
prop_delete_map = forAll (listOf genBSTop) $ \ops ->
                    toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)


-- (c) Balanced Trees
-- ------------------

-- The following function determines the `height` of a BST

height (Bind _ _ l r) = 1 + max (height l) (height r)
height Emp            = 0

-- We say that a tree is *balanced* if

isBal (Bind _ _ l r) = isBal l && isBal r && abs (height l - height r) <= 2
isBal Emp            = True

-- Write a balanced tree generator

genBal :: Gen (BST Int Char)
genBal = getBalTree 1000 4 (Bind 200 'Q' Emp Emp)

getBalTree :: Int -> Int -> (BST Int Char) -> Gen (BST Int Char)
getBalTree 0 m balTree = return balTree
getBalTree n m Emp = return Emp
getBalTree n m t@(Bind k v l r) = do nextV <- elements ['a' .. 'z']
                                     leftK <- choose (0, k - 1)
                                     rightK <- choose (k + 1, 0)
                                     let left = bstInsert leftK nextV t
                                         right = bstInsert rightK nextV t
                                     if isBal left
                                     then getBalTree (div n m) m left
                                     else if isBal right
                                     then getBalTree (div n m) m right
                                     else getBalTree (div n m) m t

-- such that

prop_genBal = forAll genBal isBal

-- (d) Height Balancing (** Hard **)
-- ---------------------------------

hDiff :: (Ord k) => BST k v -> Int
hDiff Emp = 0
hDiff (Bind k v left right) = (height left) - (height right)

rotateRight :: (Ord k) => BST k v -> BST k v
rotateRight Emp = Emp
rotateRight (Bind k v (Bind k' v' l' r') r) = Bind k' v' l' (Bind k v r' r)
rotateRight bst = bst -- otherwise

rotateLeft :: (Ord k) => BST k v -> BST k v
rotateLeft Emp = Emp
rotateLeft (Bind k v t (Bind k' v' l' r')) = Bind k' v' (Bind k v t l') r'
rotateLeft bst = bst

insBalance :: (Ord k) => k -> BST k v -> BST k v
insBalance _ Emp = Emp
insBalance k' t@(Bind k v l@(Bind kl _ _ _) r)
  | hDiff t > 1 && k' < kl = rotateRight t -- ll
  |??hDiff t > 1 && k' > kl = rotateRight (Bind k v (rotateLeft l) r) -- lr
  |??abs (hDiff t) < 2 = t
insBalance k' t@(Bind k v l r@(Bind kr _ _ _))
  | hDiff t < (-1) && k' > kr = rotateLeft t -- rr
  | hDiff t < (-1) && k' < kr = rotateLeft (Bind k v l (rotateRight r)) -- rl
  |??abs (hDiff t) < 2 = t
insBalance _ t = t

delBalance :: (Ord k) => BST k v -> BST k v
delBalance Emp = Emp
delBalance t@(Bind k v l r)
  |??hDiff t > 1    && hDiff l >= 0 = rotateRight t -- ll
  | hDiff t > 1    && hDiff l < 0  = rotateRight $ Bind k v (rotateLeft l) r
  | hDiff t < (-1) && hDiff r <= 0 = rotateLeft t -- rr
  |??hDiff t < (-1) && hDiff r > 0  = rotateLeft $ Bind k v l (rotateRight r)
  |??abs (hDiff t) < 2 = t
delBalance t = t

-- Rig it so that your insert and delete functions *also*
-- create balanced trees. That is, they satisfy the properties

prop_insert_bal ::  Property
prop_insert_bal = forAll (listOf genBSTadd) $ isBal . ofBSTops
-- >
prop_delete_bal ::  Property
prop_delete_bal = forAll (listOf genBSTop) $ isBal . ofBSTops






-- Problem 3: Circuit Testing
-- ==========================

-- Credit: [UPenn CIS552][1]

-- For this problem, you will look at a model of circuits in Haskell.

-- Signals
-- -------

-- A *signal* is a list of booleans.

newtype Signal = Sig [Bool]

-- By convention, all signals are infinite. We write a bunch of lifting
-- functions that lift boolean operators over signals.

lift0 ::  Bool -> Signal
lift0 a = Sig $ repeat a
-- >
lift1 ::  (Bool -> Bool) -> Signal -> Signal
lift1 f (Sig s) = Sig $ map f s
-- >
lift2 ::  (Bool -> Bool -> Bool) -> (Signal, Signal) -> Signal
lift2 f (Sig xs, Sig ys) = Sig $ zipWith f xs ys
-- >
lift22 :: (Bool -> Bool -> (Bool, Bool)) -> (Signal, Signal) -> (Signal,Signal)
lift22 f (Sig xs, Sig ys) =
  let (zs1,zs2) = unzip (zipWith f xs ys)
  in (Sig zs1, Sig zs2)
-- >
lift3 :: (Bool->Bool->Bool->Bool) -> (Signal, Signal, Signal) -> Signal
lift3 f (Sig xs, Sig ys, Sig zs) = Sig $ zipWith3 f xs ys zs
-- >

-- Simulation
-- ----------

-- Next, we have some helpers that can help us simulate a circuit by showing
-- how it behaves over time. For testing or printing, we truncate a signal to
-- a short prefix

truncatedSignalSize = 20
truncateSig bs = take truncatedSignalSize bs
-- >
instance Show Signal where
  show (Sig s) = show (truncateSig s) ++ "..."
-- >
trace :: [(String, Signal)] -> Int -> IO ()
trace desc count = do
  putStrLn   $ intercalate " " names
  forM_ rows $ putStrLn . intercalate " " . rowS
  where (names, wires) = unzip desc
        rows           = take count . transpose . map (\ (Sig w) -> w) $ wires
        rowS bs        = zipWith (\n b -> replicate (length n - 1) ' ' ++ (show (binary b))) names bs
-- >
probe :: [(String,Signal)] -> IO ()
probe desc = trace desc 1
-- >
simulate :: [(String, Signal)] -> IO ()
simulate desc = trace desc 20

-- Testing support (QuickCheck helpers)
-- ------------------------------------

-- Next, we have a few functions that help to generate random tests

instance Arbitrary Signal where
  arbitrary = do
    x      <- arbitrary
    Sig xs <- arbitrary
    return $ Sig (x : xs)
-- >
arbitraryListOfSize n = forM [1..n] $ \_ -> arbitrary

-- To check whether two values are equivalent

class Agreeable a where
  (===) :: a -> a -> Bool
-- >
instance Agreeable Signal where
  (Sig as) === (Sig bs) =
    all (\x->x) (zipWith (==) (truncateSig as) (truncateSig bs))
-- >
instance (Agreeable a, Agreeable b) => Agreeable (a,b) where
  (a1,b1) === (a2,b2) = (a1 === a2) && (b1 === b2)
-- >
instance Agreeable a => Agreeable [a] where
  as === bs = all id (zipWith (===) as bs)
-- >

-- To convert values from boolean to higher-level integers

class Binary a where
  binary :: a -> Integer
-- >
instance Binary Bool where
  binary b = if b then 1 else 0
-- >
instance Binary [Bool] where
  binary = foldr (\x r -> (binary x) + 2 *r) 0

-- And to probe signals at specific points.

sampleAt n (Sig b) = b !! n
sampleAtN n signals = map (sampleAt n) signals
sample1 = sampleAt 0
sampleN = sampleAtN 0


-- Basic Gates
-- -----------

-- The basic gates from which we will fashion circuits can now be described.

or2 ::  (Signal, Signal) -> Signal
or2 = lift2 $ \x y -> x || y
-- >
xor2 :: (Signal, Signal) -> Signal
xor2 = lift2 $ \x y -> (x && not y) || (not x && y)
-- >
and2 :: (Signal, Signal) -> Signal
and2 = lift2 $ \x y -> x && y
-- >
imp2 ::  (Signal, Signal) -> Signal
imp2 = lift2 $ \x y -> (not x) || y
-- >
mux :: (Signal, Signal, Signal) -> Signal
mux = lift3 (\b1 b2 select -> if select then b1 else b2)
-- >
demux :: (Signal, Signal) -> (Signal, Signal)
demux args = lift22 (\i select -> if select then (i, False) else (False, i)) args
-- >
muxN :: ([Signal], [Signal], Signal) -> [Signal]
muxN (b1,b2,sel) = map (\ (bb1,bb2) -> mux (bb1,bb2,sel)) (zip b1 b2)
-- >
demuxN :: ([Signal], Signal) -> ([Signal], [Signal])
demuxN (b,sel) = unzip (map (\bb -> demux (bb,sel)) b)


-- Basic Signals
-- -------------

-- Similarly, here are some basic signals

high = lift0 True
low  = lift0 False
-- >
str   ::  String -> Signal
str cs = Sig $ (map (== '1') cs) ++ (repeat False)
-- >
delay ::  Bool -> Signal -> Signal
delay init (Sig xs) = Sig $ init : xs


-- Combinational circuits
-- ----------------------

-- **NOTE** When you are asked to implement a circuit, you must **ONLY** use
-- the above gates or smaller circuits built from the gates.

-- For example, the following is a *half-adder* (that adds a carry-bit to a
-- single bit).

halfadd :: (Signal, Signal) -> (Signal, Signal)
halfadd (x,y) = (sum,cout)
  where sum   = xor2 (x, y)
        cout  = and2 (x, y)

-- Here is a simple property about the half-adder

prop_halfadd_commut b1 b2 =
  halfadd (lift0 b1, lift0 b2) === halfadd (lift0 b2, lift0 b1)

-- We can use the half-adder to build a full-adder

fulladd (cin, x, y) = (sum, cout)
  where (sum1, c1)  = halfadd (x,y)
        (sum, c2)   = halfadd (cin, sum1)
        cout        = xor2 (c1,c2)
-- >
test1a = probe [("cin",cin), ("x",x), ("y",y), ("  sum",sum), ("cout",cout)]
  where cin        = high
        x          = low
        y          = high
        (sum,cout) = fulladd (cin, x, y)

-- and then an n-bit adder

bitAdder :: (Signal, [Signal]) -> ([Signal], Signal)
bitAdder (cin, [])   = ([], cin)
bitAdder (cin, x:xs) = (sum:sums, cout)
  where (sum, c)     = halfadd (cin,x)
        (sums, cout) = bitAdder (c,xs)
-- >
test1 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
               ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
  where
    cin = high
    in1 = high
    in2 = high
    in3 = low
    in4 = high
    ([s1,s2,s3,s4], c) = bitAdder (cin, [in1,in2,in3,in4])

-- The correctness of the above circuit is described by the following property
-- that compares the behavior of the circuit to the *reference implementation*
-- which is an integer addition function

prop_bitAdder_Correct ::  Signal -> [Bool] -> Bool
prop_bitAdder_Correct cin xs =
  binary (sampleN out ++ [sample1 cout]) == binary xs + binary (sample1 cin)
  where (out, cout) = bitAdder (cin, map lift0 xs)

-- Finally, we can use the bit-adder to build an adder that adds two N-bit numbers

adder :: ([Signal], [Signal]) -> [Signal]
adder (xs, ys) =
   let (sums,cout) = adderAux (low, xs, ys)
   in sums ++ [cout]
   where
     adderAux (cin, [], [])     = ([], cin)
     adderAux (cin, x:xs, y:ys) = (sum:sums, cout)
                                  where (sum, c) = fulladd (cin,x,y)
                                        (sums,cout) = adderAux (c,xs,ys)
     adderAux (cin, [], ys)     = adderAux (cin, [low], ys)
     adderAux (cin, xs, [])     = adderAux (cin, xs, [low])
-- >
test2 = probe [ ("x1", x1), ("x2",x2), ("x3",x3), ("x4",x4),
                (" y1",y1), ("y2",y2), ("y3",y3), ("y4",y4),
                (" s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), (" c",c) ]
  where xs@[x1,x2,x3,x4] = [high,high,low,low]
        ys@[y1,y2,y3,y4] = [high,low,low,low]
        [s1,s2,s3,s4,c]  = adder (xs, ys)

-- And we can specify the correctness of the adder circuit by

prop_Adder_Correct ::  [Bool] -> [Bool] -> Bool
prop_Adder_Correct l1 l2 =
  binary (sampleN sum) == binary l1 + binary l2
  where sum = adder (map lift0 l1, map lift0 l2)

-- Problem: Subtraction
-- --------------------

-- 1. Using `prop_bitAdder_Correct` as a model, write a speci???cation for a
-- single-bit subtraction function that takes as inputs a N-bit binary
-- number and a single bit to be subtracted from it and yields as
-- outputs an N-bit binary number. Subtracting one from zero should
-- yield zero.

prop_bitSubtractor_Correct ::  Signal -> [Bool] -> Bool
prop_bitSubtractor_Correct bin xs = 
    case binary xs - binary (sample1 bin) of
    -1  -> (binary $ sampleN out)==0 && (binary $ sample1 bout)==1
    _   -> (binary $ sampleN out) == binary (sampleN (map lift0 xs))-binary (sample1 bin)  -- && (binary $ sample1 bout) == 0
    where (out, bout)   = bitSubtractor (bin, (map lift0 xs))

-- 2. Using the `bitAdder` circuit as a model, de???ne a `bitSubtractor`
-- circuit that implements this functionality and use QC to check that
-- your behaves correctly.

bitSubtractor :: (Signal, [Signal]) -> ([Signal], Signal)
bitSubtractor (bin, [])     = ([], bin)
bitSubtractor (bin, x:xs)   = (dif:difs, bout)
  where (tmpdif, b)         = halfsub (bin, x)
        (difs, bout)        = bitSubtractor (b, xs)
        dif                 = and2 (tmpdif, neg bout)    --when 1 is subtracted from 0

-- y-x
halfsub :: (Signal, Signal) -> (Signal, Signal)
halfsub (x, y)  = (diff, bout)
  where diff    = xor2 (x,y)
        bout    = neg $ imp2 (x,y)

{-
    truth table for halfsub to calculate y-x:
    y       = 0 0 1 1
    x       = 0 1 0 1
    diff    = 0 1 1 0
    bout    = 0 1 0 0
-}

neg :: Signal -> Signal
neg sig = lift1 (\x->not x) sig
-- Problem: Multiplication
-- -----------------------

-- 3. Using `prop_Adder_Correct` as a model, write down a QC speci???cation
-- for a `multiplier` circuit that takes two binary numbers of arbitrary
-- width as input and outputs their product.

prop_Multiplier_Correct ::  [Bool] -> [Bool] -> Bool
prop_Multiplier_Correct xs ys = 
  (binary xs * binary ys) == (binary $ sampleN prod)
  where prod = multiplier (map lift0 xs,map lift0 ys)

-- 4. De???ne a `multiplier` circuit and check that it satis???es your
-- speci???cation. (Looking at how adder is de???ned will help with this,
-- but you???ll need a little more wiring. To get an idea of how the
-- recursive structure should work, think about how to multiply two
-- binary numbers on paper.)

multiplier :: ([Signal], [Signal]) -> [Signal]
multiplier ([],_) = []
multiplier (_,[]) = []
multiplier (xs, y:ys) =
  adder (p,(rshift ps))
  where
    p   = map (myand2 y) xs
    ps  = multiplier (xs,ys)

myand2 :: Signal->Signal->Signal
myand2 sig1 sig2 = and2 (sig1,sig2)

rshift :: [Signal] -> [Signal]
rshift sigs = low:sigs
-- [1]: http://www.cis.upenn.edu/~bcpierce/courses/552-2008/resources/circuits.hs
