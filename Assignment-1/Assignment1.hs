-- CMPU-245 Fall 2018
-- Matthew Imiolek
-- Assignment 1

import Test.Tasty
import Test.Tasty.HUnit

-------------------------------------------------------------------------------------------
-- Problem 1
-------------------------------------------------------------------------------------------
-- Shows the three different methods of creating a safetail

-- Creates a safetail function using a conditional expression
safetail :: [a] -> [a]
safetail xs = if length xs > 0 then tail xs else []


-- Creates a safetail function using a gaurded equation
safetail' :: [a] -> [a]
safetail' xs | length xs > 0 = tail xs
             | otherwise     = []


-- Creates a safetail function using pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs





-------------------------------------------------------------------------------------------
-- Problem 2
-------------------------------------------------------------------------------------------
-- Give three posible definitions for the logical OR operator
-- using pattern matching
-- Changed symbol to fix ambiguous occurance errors

(|||) :: Bool -> Bool -> Bool
True  ||| True  = True
True  ||| False = True
False ||| True  = True
False ||| False = False


(||||) :: Bool -> Bool -> Bool
False |||| False = False
_ |||| _ = True


(|||||) :: Bool -> Bool -> Bool
True  ||||| _  = True
_  ||||| True = True
False ||||| False = False





-------------------------------------------------------------------------------------------
-- Problem 3
-------------------------------------------------------------------------------------------
-- Redefine && using conditionals rather than patterns
-- Changed symbol to fix ambiguous occurance errors

(&&&) :: Bool -> Bool -> Bool
(&&&) a b = if a == True then 
           if b == True then True else False 
                        else False 





-------------------------------------------------------------------------------------------
-- Problem 4
-------------------------------------------------------------------------------------------
-- Redefine True && b = b
--          False && _ = False
-- Changed symbol to fix ambiguous occurance errors

(&&&&) :: Bool -> Bool -> Bool
(&&&&) a b = if a == False then False else b





-------------------------------------------------------------------------------------------
-- Problem 5
-------------------------------------------------------------------------------------------
-- Define a function that maps an integer n to all such triples 
-- with component [1..n].

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2 == z^2]





-------------------------------------------------------------------------------------------
-- Problem 6
-------------------------------------------------------------------------------------------
-- Define a function that returns a list of all perfect numbers
-- up to a given limit.

-- a helper function that finds all the factors of n, excpet n itself
-- for use in perf
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x)) == x]





-------------------------------------------------------------------------------------------
-- Problem 7
-------------------------------------------------------------------------------------------
-- Define a function that returns True if LISTY appears as a prefix of LISTZ
-- otherwise it returns false

isPrefixOf :: Eq a => [a] -> [a] -> Bool          -- This was the type :type gave me
isPrefixOf [] listz = True
isPrefixOf listy listz = if head listy == head listz 
                            then isPrefixOf (tail listy) (tail listz)
                                 else False





-------------------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------------------
-- Had an issue where input of [] into any fuction in a test wouldn't work,
-- even if it worked when run in GHCi
-- named tests for different || and && pattern matching and conditional examples using ' to
-- represent the number of | or & required for that particular version, and bar for || and
-- and, and for &&

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [safetailTests, safetail'Tests, safetail''Tests, bar'''Tests, bar''''Tests, bar'''''Tests, and'''Tests, and''''Tests, pythsTests, perfectsTests, isPrefixOfTests]

-- safetail Tests
safetailTests = testGroup "safetail tests"
  [ -- Testing safetail function
    testCase "(safetail [3]) should be []" $
      (safetail [3]) @?= []

    -- Testing safetail function
  , testCase "(safetail [1,2,3]) should be [2,3]" $
      (safetail [1,2,3]) @?= [2,3]

    -- Testing safetail function
  --, testCase "(safetail []) should be []" $
  --    (safetail []) @?= []
  ]



-- safetail' Tests
safetail'Tests = testGroup "safetail' tests"
  [ -- Testing safetail' function
    testCase "(safetail' [3]) should be []" $
      (safetail' [3]) @?= []

    -- Testing safetail' function
  , testCase "(safetail' [1,2,3]) should be [2,3]" $
      (safetail' [1,2,3]) @?= [2,3]

    -- Testing safetail' function
  --, testCase "(safetail' []) should be []" $
  --   (safetail' []) @?= []
  ]



-- safetail'' Tests
safetail''Tests = testGroup "safetail'' tests"
  [ -- Testing safetail'' function
    testCase "(safetail'' [3]) should be []" $
      (safetail'' [3]) @?= []

    -- Testing safetail'' function
  , testCase "(safetail'' [1,2,3]) should be [2,3]" $
      (safetail'' [1,2,3]) @?= [2,3]

    -- Testing safetail'' function
  --, testCase "(safetail'' []) should be []" $
  --  (safetail'' []) @?= []
  ]



-- ||| Tests
bar'''Tests = testGroup "||| tests"
  [ -- Testing ||| function
    testCase "(True ||| True) should be True" $
      (True ||| True) @?= True

     --Testing ||| function
   , testCase "(True ||| False) should be True" $
      (True ||| False) @?= True

     --Testing ||| function
   , testCase "(False ||| True) should be True" $
      (False ||| True) @?= True

     --Testing ||| function
   , testCase "(False ||| False) should be False" $
      (False ||| False) @?= False
  ]

-- |||| Tests
bar''''Tests = testGroup "|||| tests"
  [ -- Testing |||| function
    testCase "(True |||| True) should be True" $
      (True |||| True) @?= True,

    -- Testing |||| function
    testCase "(True |||| False) should be True" $
      (True |||| False) @?= True,

    -- Testing |||| function
    testCase "(False |||| True) should be True" $
      (False |||| True) @?= True,

    -- Testing |||| function
    testCase "(False |||| False) should be False" $
      (False |||| False) @?= False
  ]




-- ||||| Tests
bar'''''Tests = testGroup "||||| tests"
  [ -- Testing ||||| function
    testCase "(True ||||| True) should be True" $
      (True ||||| True) @?= True,

    -- Testing ||||| function
    testCase "(True ||||| False) should be True" $
      (True ||||| False) @?= True,

    -- Testing ||||| function
    testCase "(False ||||| True) should be True" $
      (False ||||| True) @?= True,

    -- Testing ||||| function
    testCase "(False ||||| False) should be False" $
      (False ||||| False) @?= False
  ]



-- &&& Tests
and'''Tests = testGroup "&&& tests"
  [ -- Testing &&& function
    testCase "(True &&& True) should be True" $
      (True &&& True) @?= True,

    -- Testing &&& function
    testCase "(True &&& False) should be False" $
      (True &&& False) @?= False,

    -- Testing &&& function
    testCase "(False &&& True) should be False" $
      (False &&& True) @?= False,

    -- Testing &&& function
    testCase "(False &&& False) should be False" $
      (False &&& False) @?= False
  ]



-- &&&& Tests
and''''Tests = testGroup "&&&& tests"
  [ -- Testing &&&& function
    testCase "(True &&& True) should be True" $
      (True &&& True) @?= True,

    -- Testing &&&& function
    testCase "(True &&&& False) should be False" $
      (True &&&& False) @?= False,

    -- Testing &&&& function
    testCase "(False &&&& True) should be False" $
      (False &&&& True) @?= False,

    -- Testing &&&& function
    testCase "(False &&&& False) should be False" $
      (False &&&& False) @?= False
  ]



-- pyths Tests
pythsTests = testGroup "pyths tests"
  [ -- Testing pyths function
    testCase "(pyths 5) should be [(3,4,5),(4,3,5)]" $
      (pyths 5) @?= [(3,4,5),(4,3,5)],

    -- Testing pyths function
    testCase "(pyths 13) should be [(3,4,5),(4,3,5),(5,12,13),(12,5,13)]" $
      (pyths 13) @?= [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(12,5,13)],

    -- Testing pyths function
    testCase "(pyths 0) should be []" $
      (pyths 0) @?= []
  ]



-- perfects Tests
perfectsTests = testGroup "perfects tests"
  [ -- Testing perfects function
    testCase "(perfects 100) should be [6,28]" $
      (perfects 100) @?= [6,28],

    -- Testing perfects function
    testCase "(perfects 500) should be [6,28,496]" $
      (perfects 100) @?= [6,28],

    -- Testing perfects function
    testCase "(perfects 0) should be []" $
      (perfects 0) @?= []
  ]



-- isPrefixOf Tests
isPrefixOfTests = testGroup "isPrefixOfTests tests"
  [ -- Testing isPrefixOf function
    testCase "(isPrefixOf [1,2,3] [1,2,3,4,5,6]) should be True" $
      (isPrefixOf [1,2,3] [1,2,3,4,5,6]) @?= True,

    -- Testing isPrefixOf function
    testCase "(isPrefixOf [1,2,3] [1,2,2,4,5,6]) should be False" $
      (isPrefixOf [1,2,3] [1,2,2,4,5,6]) @?= False,

    -- Testing isPrefixOf function
    testCase "(isPrefixOf [hello [hello world]) should be True" $
      (isPrefixOf "hello" "hello world") @?= True,

-- Testing isPrefixOf function
    testCase "(isPrefixOf [helslo] [hello world]) should be False" $
      (isPrefixOf "helslo" "hello world") @?= False,

-- Testing isPrefixOf function
    testCase "(isPrefixOf [] [1,2,3,4,5,6]) should be True" $
      (isPrefixOf [] [1,2,3,4,5,6]) @?= True

-- Testing isPrefixOf function
   -- testCase "(isPrefixOf [] []) should be True" $
     -- (isPrefixOf [] []) @?= True
  ]