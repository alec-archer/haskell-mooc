module Set13b where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List


------------------------------------------------------------------------------
-- Ex 1: implement the function ifM, that takes three monadic
-- operations. If the first of the operations returns True, the second
-- operation should be run. Otherwise the third operation should be
-- run.
--
-- Note the polymorphic `Monad m =>` type signature. Your operation
-- should work on all monads, and thus needs to be implemented with
-- Monad operations like do and >>=. Don't try to pattern match on
-- Maybes.
--
-- Examples (test is defined below):
--   In the Maybe Monad:
--     ifM (Just True) (Just '1') (Just '2')  ==>  Just '1'
--     ifM (Just False) (Just '1') (Just '2') ==>  Just '2'
--     ifM Nothing (Just '1') (Just '2')      ==>  Nothing
--     ifM (Just True) (Just '1') Nothing     ==>  Just '1'
--   In the State Monad (test is defined below):
--     runState (ifM get (return 'a') (return 'b')) False
--       ==> ('b',False)
--     runState (put 11 >> ifM test (return 'a') (return 'b')) 0
--       ==> ('b',11)
--     runState (put 9 >> ifM test (return 'a') (return 'b')) 0
--       ==> ('a',9)

test :: State Int Bool
test = do
  x <- get
  return (x<10)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  bool <- opBool
  if bool then opThen else opElse

------------------------------------------------------------------------------
-- Ex 2: the standard library function Control.Monad.mapM defines a
-- monadic map operation. Some examples of using it (safeDiv is defined
-- below):
--
-- mapM (safeDiv 10.0) [1.0,5.0,2.0]  =>  Just [10.0,2.0,5.0]
-- mapM (safeDiv 10.0) [1.0,0.0,2.0]  =>  Nothing
--
-- Your task is to implement the function mapM2 that works like mapM,
-- but there are two lists and the operation takes two arguments. More
-- concretely, running `mapM2 op xs ys` should run `op`, giving it the
-- first element of xs and the first element of ys. Then, it should
-- run `op` on the second elements of xs and ys, and so forth.
-- Finally, all the values produced by `op` are returned, in order, as
-- a list.
--
-- If the lists are of different length, you can stop processing them
-- once the shorter one ends.
--
-- Examples:
--  mapM2 safeDiv [6.0,10.0,12.0] [3.0,2.0,4.0]
--    ==> Just [2.0,5.0,3.0]
--  mapM2 safeDiv [6.0,10.0,12.0] [3.0,0.0,4.0]
--    ==> Nothing
--  mapM2 (\x y -> Just (x+y)) [1,2,3] [6,7]
--    ==> Just [7,9]
--  runState (mapM2 perhapsIncrement [True,False,True] [1,2,4]) 0
--    ==> ([(),(),()],5)

-- Do not change safeDiv or perhapsIncrement, they're used by the
-- examples & test outputs.
safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0.0 = Nothing
safeDiv x y = Just (x/y)

perhapsIncrement :: Bool -> Int -> State Int ()
perhapsIncrement True x = modify (+x)
perhapsIncrement False _ = return ()

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 _ [] _ = return []
mapM2 _ _ [] = return []
mapM2 op (x:xs) (y:ys) = op x y >>= \result ->
                         mapM2 op xs ys >>= \rest ->
                         return (result:rest)
-- mapM2 op (x:xs) (y:ys) = do
--   result <- op x y
--   rest <- mapM2 op xs ys
--   return (result:rest)

  
-- mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- mapM2 op xs ys = return (go xs ys)
--   where go _ [] = []
--         go [] _ = []
--         go (x:xs) (y:ys) = do -- do means that do returns m X -- what happens in monad, stays in monad
--           result <- op x y
--           result:(go xs ys)
                            
-- another attempt where I define type signature of go; got same error as above attempt; b/c I unwrap `m c` in do block (`m c` is return type of `op`), 
-- actual return type for go is `m c` even though expected return type is [c]; do block must return monad which makes sense b/c do block is a substitute for
-- using >>=. the >>= operator returns a monad (m a -> (a -> m b) -> m b)
-- mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- mapM2 op xs ys = return (go op xs ys)

-- go :: Monad m => (a -> b -> m c) -> [a] -> [b] -> [c]
-- go _ _ [] = []
-- go _ [] _ = []
-- go op (x:xs) (y:ys) = do 
--   result <- op x y
--   result:(go op xs ys)
------------------------------------------------------------------------------
-- Ex 3: Finding paths.
--
-- In this exercise, you'll process mazes, described as lists like this:

maze1 :: [(String,[String])]
maze1 = [("Entry",["Pit","Corridor 1"])
        ,("Pit",[])
        ,("Corridor 1",["Entry","Dead end"])
        ,("Dead end",["Corridor 1"])
        ,("Corridor 2",["Corridor 3"])
        ,("Corridor 3",["Corridor 2"])]

-- This means that you can get from Entry to Pit or Corridor 1, and
-- from Corridor 1 you can get back to Entry or the Dead end, and so
-- forth. Here's a drawing of what maze1 looks like. Note how you
-- can't get out of the Pit, and Corridors 2 and 3 aren't connected to
-- the Entry.
--
--  Entry <--> Corridor 1 <--> Dead end
--   |
--   v         Corridor 2 <--> Corridor 3
--  Pit
--
-- Your task is to implement the function path that checks if there is
-- a path from one location to another.
--
--   path maze1 "Entry" "Pit"        ==> True
--   path maze1 "Entry" "Dead end"   ==> True
--   path maze1 "Pit"   "Entry"      ==> False
--   path maze1 "Entry" "Corridor 2" ==> False
--
-- To implement path, we'll need some helper functions. We'll work in
-- the State monad, with a state of type [String]. This tracks which
-- places we've been to.
--
-- The operation `visit maze place1` should work like this:
--   * if place1 is in the state (i.e. we've visited it before), do nothing
--   * otherwise, add place1 to the state (it has now been visited), and:
--      * for all neighbouring places of place1, run visit
--
-- PS. You might recognize this as a Depth-First Search, but if you
-- haven't heard the term, don't worry.
--
-- Examples:
--   runState (visit maze1 "Pit") []
--     ==> ((),["Pit"])
--   runState (visit maze1 "Corridor 2") []
--     ==> ((),["Corridor 3","Corridor 2"])
--   runState (visit maze1 "Entry") []
--     ==> ((),["Dead end","Corridor 1","Pit","Entry"])
--   runState (visit maze1 "Entry") ["Corridor 1"]
--     ==> ((),["Pit","Entry","Corridor 1"])


visit :: [(String,[String])] -> String -> State [String] ()
visit maze place = do
  state <- get
  case (find (==place) state) of
    Nothing -> do
                 put (place:state)
                 case (lookup place maze) of
                  Just neighbours -> mapM_ (visit maze) neighbours
                  Nothing -> return ()
    Just _ -> return ()

-- Now you should be able to implement path using visit. If you run
-- visit on a place using an empty state, you'll get a state that
-- lists all the places that are reachable from the starting place.

path :: [(String,[String])] -> String -> String -> Bool
path maze place1 place2 = case find (==place2) (snd (runState (visit maze place1) [])) of
                            Just _ -> True
                            Nothing -> False


------------------------------------------------------------------------------
-- Ex 4: Given two lists, ks and ns, find numbers i and j from ks,
-- such that their sum i+j=n is in ns. Return all such triples
-- (i,j,n).
--
-- Use the list monad!
--
-- Examples:
--  findSum2 [1,2,3,4] [6,7]
--    ==> [(2,4,6),(3,3,6),(3,4,7),(4,2,6),(4,3,7)]
--
-- PS. The tests don't care about the order of results.

findSum2 :: [Int] -> [Int] -> [(Int,Int,Int)]
-- findSum2 ks ns = do
--   i <- ks
--   j <- ks
--   n <- ns 
--   if i + j == n then [(i,j,n)] else []

findSum2 ks ns = 
  ks >>= \i -> -- need to use >>= instead of >> b/c we need value i
  ks >>= \j -> 
  ns >>= \n ->
  if i + j == n then [(i,j,n)] else [] -- don't need return fnc because [] is our monad instance; doing return [(i,j,n)] would return [[(i,j,n)]]

------------------------------------------------------------------------------
-- Ex 5: compute all possible sums of elements from the given
-- list. Use the list monad.
--
-- Hint! a list literal like [True,False] or [x,0] can be useful when
-- combined with do-notation!
--
-- The order of the returned list does not matter and it may contain
-- duplicates.
--
-- Examples:
--   allSums []
--     ==> [0]
--   allSums [1]
--     ==> [1,0]
--   allSums [1,2,4]
--     ==> [7,3,5,1,6,2,4,0]

subsequence :: [Int] -> [[Int]]
subsequence [] = [[0]]
subsequence (x:xs) = map (x:) (subsequence xs) ++ subsequence xs  

allSums :: [Int] -> [Int]
allSums [] = [0]
allSums xs = map sum (subsequence xs) 

-- allSums :: [Int] -> [Int]
-- allSums [] = [0]
-- allSums (x:xs) = [sum (x:xs)] ++ go x xs ++ allSums xs
--   where go _ [] = [] 
--         go x (y:ys) = (x+y):go x ys

------------------------------------------------------------------------------
-- Ex 6: the standard library defines the function
--
--   foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--
-- This function behaves like foldr, but the operation used is
-- monadic. foldM f acc xs works by running f for each element in xs,
-- giving it also the result of the previous invocation of f.
--
-- Your task is to implement the functions f1 and f2 so that the
-- functions sumBounded and sumNotTwice work.
--
-- Do not change the definitions of sumBounded and sumNotTwice. The
-- tests have their own copies of the definitions anyway.

-- sumBounded computes the sum of a list. However if the sum at any
-- point during the execution goes over the given bound, Nothing is
-- returned.
--
-- Examples:
--  sumBounded 5 [1,2,1,-2,3]
--    ==> Just 5
--  sumBounded 5 [1,2,3,1,-2]   -- 1+2+3=6 which results in Nothing
--    ==> Nothing
sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x 
  | sum > k = Nothing
  | otherwise = Just sum
  where sum = acc + x

-- sumNotTwice computes the sum of a list, but counts only the first
-- occurrence of each value.
--
-- Examples:
--  sumNotTwice [1,2,3]          ==> 6
--  sumNotTwice [1,1,2,3,2,2,3]  ==> 6
--  sumNotTwice [3,-2,3]         ==> 1
--  sumNotTwice [1,2,-2,3]       ==> 4
sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 0 xs) []

f2 :: Int -> Int -> State [Int] Int
f2 acc x = do
  state <- get
  case (find (x==) state) of
    Just _ -> return acc
    Nothing -> do
                 put (x:state)
                 return (acc + x)

------------------------------------------------------------------------------
-- Ex 7: here is the Result type from Set12. Implement a Monad Result
-- instance that behaves roughly like the Monad Maybe instance.
--
-- That is,
--   1. MkResult behave like Just
--   2. If part of computation produces NoResult, the whole computation
--      produces NoResult (just like Nothing)
--   3. Similarly, if we get a Failure "reason" value, the whole
--      computation produces Failure "reason"
--
-- Examples:
--   MkResult 1 >> Failure "boom" >> MkResult 2
--     ==> Failure "boom"
--   MkResult 1 >> NoResult >> Failure "not reached"
--     ==> NoResult
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

data Result a = MkResult a | NoResult | Failure String deriving (Show,Eq)

instance Functor Result where
  -- The same Functor instance you used in Set12 works here.
  fmap _ NoResult = NoResult
  fmap _ (Failure str) = Failure str
  fmap f (MkResult a) = MkResult (f a)

-- This is an Applicative instance that works for any monad, you
-- can just ignore it for now. We'll get back to Applicative later.
instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  -- implement return and >>=
  return  x = MkResult x
  NoResult >>= _ = NoResult
  Failure str >>= _ = Failure str
  MkResult x >>= f = f x 

------------------------------------------------------------------------------
-- Ex 8: Here is the type SL that combines the State and Logger
-- types. Implement an instance Monad SL, that behaves like the
-- combination of State and Logger. That is, state is propagated from
-- one operation to the next, and log messages are stored in the order
-- they are produced.
--
-- To simplify the type signatures, the type of the state has been set
-- to Int, instead of being a parameter like in the standard State
-- monad.
--
-- This is a tough one, probably the hardest exercise on this course!
-- You can come back to it later if you don't get it now.
--
-- You might find it easier to start with the Functor instance
--
-- Examples:
--   runSL (putSL 2 >> msgSL "hello" >> getSL) 0
--      ==> (2,2,["hello"])
--   runSL (replicateM_ 5 (modifySL (+1) >> getSL >>= \x -> msgSL ("got "++show x))) 1
--      ==> ((),6,["got 2","got 3","got 4","got 5","got 6"])

data SL a = SL (Int -> (a,Int,[String]))

-- Run an SL operation with the given starting state
runSL :: SL a -> Int -> (a,Int,[String])
runSL (SL f) state = f state

-- Write a log message
msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((),s,[msg]))

-- Fetch the state
getSL :: SL Int
getSL = SL (\s -> (s,s,[]))

-- Overwrite the state
putSL :: Int -> SL ()
putSL s' = SL (\s -> ((),s',[]))

-- Modify the state
modifySL :: (Int->Int) -> SL ()
modifySL f = SL (\s -> ((),f s,[]))

fst' :: (a, b, c) -> a
fst' (a, _, _) = a
snd' :: (a, b, c) -> b 
snd' (_, b, _) = b
thrd' :: (a, b, c) -> c 
thrd' (_,_,c) = c

instance Functor SL where
  -- implement fmap
  -- fmap f (SL g) = SL g'
  --   where g' x = (f (fst' (g x)), snd' (g x), thrd' (g x))

  -- alternative 1
  -- fmap f (SL g) = SL h
  --   where h x = let (val, state, log) = g x
  --               in (f val, state, log)

  -- alternative 2
  fmap f (SL g) = SL (\s -> let (val, state, log) = g s in (f val, state, log))

-- This is an Applicative instance that works for any monad, you
-- can just ignore it for now. We'll get back to Applicative later.
instance Applicative SL where
  pure = return
  (<*>) = ap

instance Monad SL where
  -- implement return and >>=
  return x = SL (\s -> (x, s, []))
  -- return x = SL f
    -- where f y = (x, snd' (f y), thrd' (f y))
  op >>= f = SL h
    where h state0 = let (val, state1, log1) = runSL op state0
                         op2 = f val
                         (val2, state2, log2) = runSL op2 state1
                     in (val2, state2, log1 ++ log2) 

------------------------------------------------------------------------------
-- Ex 9: Implement the operation mkCounter that produces the IO operations
-- inc :: IO () and get :: IO Int. These operations should work like this:
--
--   get returns the number of times inc has been called
--
-- In other words, a simple stateful counter. Use an IORef to store the count.
--
-- Note: this is an IO operation that produces two IO operations. Thus
-- the type of mkCounter is IO (IO (), IO Int).
--
-- This exercise is tricky. Feel free to leave it until later.
--
-- An example of how mkCounter works in GHCi:
--
--  *Set11b> (inc,get) <- mkCounter
--  *Set11b> inc
--  *Set11b> inc
--  *Set11b> get
--  2
--  *Set11b> inc
--  *Set11b> inc
--  *Set11b> get
--  4

mkCounter :: IO (IO (), IO Int)
mkCounter = do
  ref <- newIORef 0 
  return (modifyIORef ref (+1), readIORef ref)
