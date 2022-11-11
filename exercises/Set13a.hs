module Set13a where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map

import Examples.Bank


------------------------------------------------------------------------------
-- Ex 1: Your task is to help implement the function readName that
-- given a string like "Forename Surname" produces the pair
-- ("Forename", "Surname"). readName should fail (return Nothing) in
-- the following cases:
--
--   1. the input string doesn't contain a space
--   2. one of the names contains numbers
--   3. one of the names doesn't start with a capital letter
--
-- The function readNames has already been implemented using the ?>
-- operator from the course material. You need to define the helper
-- functions split, checkNumber and checkCapitals so that readNames
-- works correctly.

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing   -- In case of failure, propagate failure
Just x  ?> f = f x       -- In case of success, run the next computation

-- DO NOT touch this definition!
readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkNumber
  ?>
  checkCapitals

-- split should split a string into two words. If the input doesn't
-- contain a space, Nothing should be returned
--
-- (NB! There are obviously other corner cases like the inputs " " and
-- "a b c", but you don't need to worry about those here)
split :: String -> Maybe (String,String)
split s = case (mySplit ' ' s) of
            [x,y] -> Just (x,y)
            _ -> Nothing

mySplit :: Char -> String -> [String]
mySplit c xs = helper [] xs
  where helper piece [] = [piece]
        helper piece (y:ys)
          | c == y    = piece : helper [] ys
          | otherwise = helper (piece++[y]) ys

-- checkNumber should take a pair of two strings and return them
-- unchanged if they don't contain numbers. Otherwise Nothing is
-- returned.
checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (first, last)
  | any hasNumber first || any hasNumber last = Nothing
  | otherwise = Just (first, last) 
  where hasNumber s = elem s "0123456789" 

-- checkCapitals should take a pair of two strings and return them
-- unchanged if both start with a capital letter. Otherwise Nothing is
-- returned.
checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (for,sur)
  | isCapitalized (head for) && isCapitalized (head sur) = Just (for, sur)
  | otherwise = Nothing
  where isCapitalized c = elem c "QWERTYUIOPASDFGHJKLZXCVBNM"

------------------------------------------------------------------------------
-- Ex 2: Given a list of players and their scores (as [(String,Int)]),
-- and two player names, return the name of the player who has more
-- points (wrapped in a Just), or Nothing if either of the players
-- doesn't exist.
--
-- In the case of a draw, prefer the first player.
--
-- Use the function
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- and either do-notation (easier) or ?> chaining (trickier!)
--
-- Examples:
--   winner [("ender",13),("orson",6),("scott",5)] "ender" "orson"
--     ==> Just "ender"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "ender"
--     ==> Just "ender"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "scott"
--     ==> Just "orson"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "ridley"
--     ==> Nothing
--   winner [("a",1),("b",1)] "a" "b"
--     ==> Just "a"

winner :: [(String,Int)] -> String -> String -> Maybe String
winner scores player1 player2 = do
  player1Score <- lookup player1 scores
  player2Score <- lookup player2 scores 
  if player1Score >= player2Score
    then return player1
    else return player2

------------------------------------------------------------------------------
-- Ex 3: given a list of indices and a list of values, return the sum
-- of the values in the given indices. You should fail if any of the
-- indices is too large or too small.
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- Hint! implement a function safeIndex :: [a] -> Int -> Maybe a
--
-- Examples:
--  selectSum [0..10] [4,6,9]
--    Just 19
--  selectSum [0..10] [4,6,9,20]
--    Nothing

-- selectSum :: Num a => [a] -> [Int] -> Maybe a
-- selectSum xs is = do
--   vals <- mapM (safeIndex xs) is
--   return (sum vals)

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = mapM (safeIndex xs) is >>= \x ->
                  return (sum x)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i > (length xs - 1) || i < 0 = Nothing
  | otherwise = Just (xs !! i)
------------------------------------------------------------------------------
-- Ex 4: Here is the Logger monad from the course material. Implement
-- the operation countAndLog which produces the number of elements
-- from the given list that fulfil the given predicate. Additionally,
-- countAndLog should log all elements that fulfil the predicate
-- (using show to turn them into strings).
--
-- Examples:
--   countAndLog even [0,1,2,3,4,5]
--     ==> Logger ["0","2","4"] 3

data Logger a = Logger [String] a
  deriving (Show, Eq)

msg :: String -> Logger ()
msg s = Logger [s] ()

instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

-- This is an Applicative instance that works for any monad, you
-- can just ignore it for now. We'll get back to Applicative later.
instance Applicative Logger where
  pure = return
  (<*>) = ap

countAndLog :: Show a => (a -> Bool) -> [a] -> Logger Int
countAndLog _ [] = return 0
-- countAndLog f (x:xs)
--   | f x = do msg (show x) 
--              sum <- countAndLog f xs
--              return (1 + sum)
--   | otherwise = do countAndLog f xs
countAndLog f (x:xs)
  | f x = msg (show x) >> -- >> b/c we don't care about value from `msg (show x)` which is ()
          countAndLog f xs >>= \sum ->
          return (1 + sum)
  | otherwise = countAndLog f xs

------------------------------------------------------------------------------
-- Ex 5: You can find the Bank and BankOp code from the course
-- material in the module Examples.Bank (file
-- exercises/Examples/Bank.hs), which has been imported into this
-- namespace.
--
-- Implement a BankOp balance that produces the balance of the given
-- account. Produce 0 if the account does not exist. The balance
-- operation shouldn't change the state of the Bank. The functions
-- from Data.Map are available under the prefix Map.

exampleBank :: Bank
exampleBank = (Bank (Map.fromList [("harry",10),("cedric",7),("ginny",1)]))

balance :: String -> BankOp Int
balance accountName = BankOp balance' 
  where balance' (Bank bank) = (Map.findWithDefault 0 accountName bank, Bank bank)

------------------------------------------------------------------------------
-- Ex 6: Using the operations balance, withdrawOp and depositOp, and
-- chaining (+>), implement the BankOp rob, which transfers all the
-- money from one account to another account.
--
-- Examples:
--   runBankOp (balance "harry") exampleBank
--     ==> (10,Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))
--   runBankOp (balance "sean") exampleBank
--     ==> (0,Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))
--   runBankOp (rob "cedric" "ginny") exampleBank
--     ==> ((),Bank (fromList [("cedric",0),("ginny",8),("harry",10)]))
--   runBankOp (rob "sean" "ginny") exampleBank
--     ==> ((),Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))

rob :: String -> String -> BankOp ()
rob from to = balance from +>
              withdrawOp from +>
              depositOp to

------------------------------------------------------------------------------
-- Ex 7: using the State monad, write the operation `update` that first
-- multiplies the state by 2 and then adds one to it. The state has
-- type Int.
--
-- Example:
--  runState update 3
--    ==> ((),7)

update :: State Int ()
-- update = do old <- get
--             put (old*2 + 1)

update = get >>= \old ->
         put (old*2 + 1)

------------------------------------------------------------------------------
-- Ex 8: Checking that parentheses are balanced with the State monad.
--
-- Do this by implementing the function paren, which updates the state
-- based on a single character. A '(' should increase the state, and a
-- ')' should decrease the state. If the state goes to -1 (there are
-- more closing than opening parentheses), it should stay there to
-- indicate that a parenthesis error was encountered.
--
-- After you've implemented paren, the given definition of parensMatch
-- should work.
--
-- Examples:
--   runState (paren '(') 3    ==> ((),4)
--   runState (paren ')') 3    ==> ((),2)
--   runState (paren ')') 0    ==> ((),-1)
--   runState (paren ')') (-1) ==> ((),-1)
--   runState (paren '(') (-1) ==> ((),-1)
--   parensMatch "()"          ==> True
--   parensMatch "("           ==> False
--   parensMatch "())"         ==> False
--   parensMatch "(()(()()))"  ==> True
--   parensMatch "(()((()))"   ==> False
--   parensMatch "(()))("      ==> False

paren :: Char -> State Int ()
paren c = do old <- get
             if old < 0
               then put old 
               else
                 case c of
                   '(' -> put (old + 1)
                   ')' -> put (old - 1)
                   _ -> return ()

parensMatch :: String -> Bool
parensMatch s = count == 0
  where (_,count) = runState (mapM_ paren s) 0

------------------------------------------------------------------------------
-- Ex 9: using a state of type [(a,Int)] we can keep track of the
-- numbers of occurrences of elements of type a. For instance
-- [('a',1),('x',3)] means that we've seen one 'a' and three 'x's.
--
-- Implement a State monad operation count that registers the
-- occurrence of the given value.
--
-- That is, the operation `count x` should fetch the pair `(x,n)` from
-- the state, and replace it with the pair `(x,n+1)`. If no such pair
-- is found, the operation should add `(x,1)` to the state.
--
-- Examples:
--  runState (count True) []
--    ==> ((),[(True,1)])
--  runState (count 7) []
--    ==> ((),[(7,1)])
--  runState (count 'a') [('a',1),('b',3)]
--    ==> ((),[('a',2),('b',3)])
--  runState (count 'a' >> count 'b' >> count 'a') []
--    ==> ((),[('a',2),('b',1)])
--
-- PS. The order of the list of pairs doesn't matter

count :: Eq a => a -> State [(a,Int)] ()
count x = do old <- get
             case (lookup x old) of
               Just val -> modify (update val)
               Nothing -> put ((x,1):old)
               where update val xs = (x, val + 1):(filter (\(k,v) -> k /= x) xs) -- filter is not the most efficient; could use findIndex, take, then concatenate

------------------------------------------------------------------------------
-- Ex 10: Implement the operation occurrences, which
--   1. runs the count operation on each element in the input list
--   2. finally produces the number of different items stored in the
--      state
--
-- In other words, use the state monad to count how many unique values
-- occur in a list.
--
-- Examples:
--  runState (occurrences [True,True,True,False,False]) []
--    ==> (2,[(True,3),(False,2)])
--  runState (occurrences [5,5,6,6,5,6,7]) []
--    ==> (3,[(5,3),(6,3),(7,1)])
--  runState (occurrences [True,False]) [(True,1)]
--    ==> (2,[(True,2),(False,1)])
--  runState (occurrences [4,7]) [(2,1),(3,1)]
--    ==> (4,[(2,1),(3,1),(4,1),(7,1)])

occurrences :: (Eq a) => [a] -> State [(a,Int)] Int
occurrences xs = do mapM_ count xs 
                    state <- get
                    return (length state)
