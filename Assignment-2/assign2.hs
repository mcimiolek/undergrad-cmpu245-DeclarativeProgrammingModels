-- CMPU-245 Fall 2018
-- Matthew Imiolek
-- Assignment 2 (Mid-Term)

import Test.Tasty
import Test.Tasty.HUnit

----------------------------------------------------------------------
-- Given Haskell data type declaration for a diff-tree
----------------------------------------------------------------------
data Diff_tree = One | Diff Diff_tree Diff_tree
 
instance Show Diff_tree where
  show One          = "(One)"
  show (Diff n1 n2) = "(Diff " ++ show n1 ++ " " ++ show n2 ++ ")"






----------------------------------------------------------------------
-- Question 1
----------------------------------------------------------------------
-- Every number has infinitely many representations in this system
-- because diff-trees work by calculating the difference between two
-- sides of a tree. Because of this system it is possible to build up
-- the sides of each problem to whatever you want, and as long as the
-- final difference between the two final sides is the same, then the
-- result will be correct. However, this also means that as long as
-- both sides increase or decrease at the same rate the difference
-- will be the same, and therefore as the value represented by both
-- sides can have infinite values with the same difference, there are
-- infinite representations of the same number.






----------------------------------------------------------------------
-- Question 2
----------------------------------------------------------------------
-- implement the functional interface for Natural Numbers using the
-- above Diff_tree data type.

-- zero function, which turns any value into <0>.
zero :: Int -> Diff_tree
zero a = (Diff (One) (One))



-- is_zero function, which checks if any value is 0.
-- is_zero :: Diff_tree -> Boolean
is_zero a | value_of(a) == 0 = True
          | otherwise        = False



-- successor function, which gives the next number of the given diff-tree
successor a = (Diff (a) (Diff (Diff One One) One))



-- predecessor function, which gives te previous number of the given
-- dif-tree
predecessor a = (Diff (a) (One))






----------------------------------------------------------------------
-- Question 3
----------------------------------------------------------------------
-- write a value_of function that converts a Diff_tree into an actual
-- number.
value_of :: Diff_tree -> Int
value_of One = 1
value_of (Diff n1 n2)  = value_of n1 - value_of n2






----------------------------------------------------------------------
-- Question 4
----------------------------------------------------------------------
-- write a int2dt function that converts a Haskell number into a 
-- Diff_tree.
int2dt :: Int -> Diff_tree
int2dt a | a > 0  = successor (int2dt(a - 1))
         | a < 0  = predecessor (int2dt(a + 1))
         | a == 0 = (Diff One One)






----------------------------------------------------------------------
-- Question 5
----------------------------------------------------------------------
-- write a diff_tree_plus function and a diff_tree_minus function

-- diff_tree_plus adds the values of the first and second diff trees
-- input together
diff_tree_plus :: Diff_tree -> Diff_tree -> Diff_tree
diff_tree_plus a b = (Diff a (Diff (Diff One One) b))



-- diff_tree_minus subtracts the values of the second diff tree from
-- the first
diff_tree_minus :: Diff_tree -> Diff_tree -> Diff_tree
diff_tree_minus a b = (Diff a b)






----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------
main = defaultMain tests
 
tests :: TestTree
tests = testGroup "Diff_tree Tests" [conversionTests, interfaceTests, mathTests]
 
conversionTests = testGroup "conversions between Int and Diff_tree"
  [ -- Testing value_of, zero, and int2dt
    testCase "(value_of One) should be 1" $
      (value_of One) @?= 1
    ,
    testCase "(value_of (zero 42)) should be 0" $
      (value_of (zero 42)) @?= 0
    ,
    testCase "(value_of (zero (-5))) should be 0" $
      (value_of (zero (-5))) @?= 0
    ,
    testCase "(value_of (int2dt 42)) should be 42" $
      (value_of (int2dt 42)) @?= 42
    ,
    testCase "(value_of (int2dt 0)) should be 0" $
      (value_of (int2dt 0)) @?= 0
    ,
    testCase "(value_of (int2dt (-5))) should be -5" $
      (value_of (int2dt (-5))) @?= (-5)
  ]
 
interfaceTests = testGroup "Natural number interface tests for Diff_trees"
  [ -- Testing is_zero, successor, and predecessor functions
    testCase "(value_of (successor One)) should be 2" $
      (value_of (successor One)) @?= 2
    ,
    testCase "(value_of (predessor One)) should be 0" $
      (value_of (predecessor One)) @?= 0
    ,
    testCase "(value_of (successor (int2dt (-1)))) should be 0" $
      (value_of (successor (int2dt (-1)))) @?= 0
    , 
    testCase "(value_of (predecessor One)) should be 0" $ 
      (value_of (predecessor One)) @?= 0
    ,
    testCase "(value_of (predecessor (int2dt (-1)))) should be -2" $
      (value_of (predecessor (int2dt (-1)))) @?= (-2)
    ,
    testCase "(value_of (successor (int2dt(0)))) should be 1" $
      (value_of (successor (int2dt(0)))) @?= 1
    ,
    testCase "(value_of (predessor (int2dt(0)))) should be -1" $
      (value_of (predecessor (int2dt(0)))) @?= (-1)
    ,
    testCase "(is_zero (int2dt(0))) should be True" $
      (is_zero (int2dt(0)))  @?= True
    ,
    testCase "(is_zero (int2dt(-1))) should be False" $
      (is_zero (int2dt(-1)))  @?= False
    ,
    testCase "(is_zero (int2dt(1))) should be False" $
      (is_zero (int2dt(1)))  @?= False
  ] 

mathTests = testGroup "Tests for diff_tree_plus and diff_tree_minus"
  [ -- Testing for diff_tree_plus and diff_tree_minus
    testCase "(value_of(diff_tree_plus (int2dt(10)) (int2dt(10)))) should be 20" $
      (value_of(diff_tree_plus (int2dt(10)) (int2dt(10)))) @?= 20
    ,
    testCase "(value_of(diff_tree_plus (int2dt(-10)) (int2dt(-10)))) should be -20" $
      (value_of(diff_tree_plus (int2dt(-10)) (int2dt(-10)))) @?= (-20)
    ,
    testCase "(value_of(diff_tree_plus (int2dt(10)) (int2dt(-10)))) should be 0" $
      (value_of(diff_tree_plus (int2dt(10)) (int2dt(-10)))) @?= 0
    ,
    testCase "(value_of(diff_tree_plus (int2dt(-10)) (int2dt(10)))) should be 0" $
      (value_of(diff_tree_plus (int2dt(-10)) (int2dt(10)))) @?= 0
    ,
    testCase "(value_of(diff_tree_plus (int2dt(10)) (int2dt(-5)))) should be 5" $
      (value_of(diff_tree_plus (int2dt(10)) (int2dt(-5)))) @?= 5
    ,
    testCase "(value_of(diff_tree_plus (int2dt(-5)) (int2dt(10)))) should be 5" $
      (value_of(diff_tree_plus (int2dt(-5)) (int2dt(10)))) @?= 5
    ,
     testCase "(value_of(diff_tree_minus (int2dt(10)) (int2dt(10)))) should be 0" $
      (value_of(diff_tree_minus (int2dt(10)) (int2dt(10)))) @?= 0
    ,
    testCase "(value_of(diff_tree_minus (int2dt(-10)) (int2dt(-10)))) should be 0" $
      (value_of(diff_tree_minus (int2dt(-10)) (int2dt(-10)))) @?= (0)
    ,
    testCase "(value_of(diff_tree_minus (int2dt(10)) (int2dt(-10)))) should be 20" $
      (value_of(diff_tree_minus (int2dt(10)) (int2dt(-10)))) @?= 20
    ,
    testCase "(value_of(diff_tree_minus (int2dt(-10)) (int2dt(10)))) should be -20" $
      (value_of(diff_tree_minus (int2dt(-10)) (int2dt(10)))) @?= (-20)
    ,
    testCase "(value_of(diff_tree_minus (int2dt(10)) (int2dt(-5)))) should be 15" $
      (value_of(diff_tree_minus (int2dt(10)) (int2dt(-5)))) @?= 15
    ,
    testCase "(value_of(diff_tree_minus (int2dt(-5)) (int2dt(10)))) should be -15" $
      (value_of(diff_tree_minus (int2dt(-5)) (int2dt(10)))) @?= (-15)
   ]