{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Control.Exception (SomeException (SomeException))
import Data.Maybe (isNothing)
import Data.List (isSubsequenceOf)
import Numeric.Natural

import qualified P99.Lists as P99
import qualified P99.Sx as Sx
import P99.Sx (Sx(..))

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Int)

main :: IO ()
main = hspec $ do
  describe "P01 (*) Find the last element of a list" $ do

    it "returns the last element of an *any* list" $
      property $ \case
        l@[] -> isNothing (P99.myLast l)
        l    -> P99.myLast l == Just (last l :: Int)

  describe "P02 (*) Find the last but one element of a list. " $ do
    describe "lastBut1" $ do
      prop "returns the second-last element of a big enough list" $
        \l -> (length l >= 2) ==> P99.lastBut1 (l:: [Int])
              `shouldBe` Just (head $ tail $ reverse l)
      prop "should return Nothing for a singleton list" $
        \a -> P99.lastBut1 [a::Int] `shouldBe` Nothing
      it "should return Nothing for an empty list" $ do
        P99.lastBut1 ([]::[Int]) `shouldBe` Nothing

  describe "P03 (*) Find the K'th element of a list. " $ do
    describe "elementAt" $ do
      prop "return the kth element of any list." $
        \l i -> P99.elementAt i (l::[Int]) `shouldBe`
                lookup i ([1..] `zip` l)

  describe "P04 (*) Find the number of elements of a list." $ do
    describe "numElements" $ do
      prop "must give you the length of the list" $
        \l -> P99.numElements l `shouldBe` length (l :: String)

  describe "P05 (*) Reverse a list." $ do
    describe "myReverse" $ do
      prop "must reverse any list (like reverse)" $
        \l -> P99.myReverse l `shouldBe` reverse (l :: [Double])

  describe "P06 (*) Find out whether a list is a palindrome. " $ do
    describe "isPalindrome" $ do
      prop "any list appended to it's reverse should be seen as a palindrome" $
        \l -> let l' = (l :: String) ++ reverse l in
              P99.isPalindrome l' `shouldBe` True
      prop "any list appended to it's reverse sandwiching an element should be seen as a palindrome" $
        \l a -> let l' = (l :: String) ++ [a] ++ reverse l in
              P99.isPalindrome l' `shouldBe` True
      prop "any singleton list  should be seen as a palindrome" $
        \a -> P99.isPalindrome [a::Int] `shouldBe` True

  describe "P07 (**) Flatten a nested list structure. " $
    let  n :: Int -> Sx Int
         n = Vx
         isList = Sx.isList
         append = Sx.append
         bits ss got = all (`isSubsequenceOf` got) ss
         errLike msgMatch (SomeException msg) = msgMatch $ show msg
         fromL (x:xs) = Vx x :~ fromL xs
         fromL [] = NIL
         flatten = P99.flatten
    in do
    describe "S-expressions" $ do
      describe "show" $ do
        it "show should render lists correctly" $ do
          show (NIL :: Sx Int) `shouldBe` "()"
          show (n 1 :~ n 2 :~ n 4 :~ NIL) `shouldBe` "(1 2 4)"
          show (n 1 :~ (n 2 :~ NIL) :~ n 4 :~ NIL) `shouldBe` "(1 (2) 4)"
          show (n 3 :~ n 1) `shouldBe` "(3 . 1)"
        prop "show should render values correctly" $
          \i -> show (n i) `shouldBe` show i
      describe "isList" $ do
        it "should detect any valid list" $ do
          isList NIL `shouldBe` True
          isList (n 1 :~ NIL) `shouldBe` True
          isList ((n 1 :~ n 2) :~ NIL) `shouldBe` True
        it "should reject any non-list" $ do
          isList (n 7) `shouldBe` False
          isList (Vx True) `shouldBe` False
          isList (n 1 :~ n 2) `shouldBe` False
      describe "append" $ do
        it "should reject non-lists" $ do
          append (n 1) NIL `shouldThrow` errLike (bits ["Not a list", ": 1"])
          append NIL (n 2 :~ n 3) `shouldThrow` errLike (bits ["Not", "list", "(2 . 3)"])
        prop "should append lists" $
          \(l1, l2) -> do l3' <- append (fromL l1) (fromL l2)
                          l3' `shouldBe` fromL (l1 ++ l2)
    describe "flatten" $ do
      it "should reject non-lists" $ do
        flatten (n 1) `shouldThrow` errLike (bits ["non-list", ": 1"])
        flatten (n 2 :~ n 3) `shouldThrow` errLike (bits ["non", "list:3"])
      prop "should do nothing if given an already flat list" $
        \l -> do fl <- P99.flatten (fromL (l :: [Int]))
                 fl `shouldBe` fromL l
      it "should flatten nested lists" $
        let l1 = (n 1 :~ fromL [23, 45] :~ ((n 7 :~ fromL [8, 11] :~ NIL) :~ n 13 :~ NIL) :~ NIL)
        in do
          show l1 `shouldBe` "(1 (23 45) ((7 (8 11)) 13))"
          fl1 <- flatten l1
          fl1 `shouldBe` fromL [1, 23, 45, 7, 8, 11, 13]
