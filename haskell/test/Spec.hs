{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Control.Exception (SomeException (SomeException))
import Data.Maybe (isNothing)
import Data.List (isSubsequenceOf, group)
import Numeric.Natural

import qualified P99.Lists as P99

import qualified P99.Sx as Sx
import P99.Sx (Sx(Vx))

import qualified P99.Sxpr as Sxpr
import P99.Sxpr (Sxpr(..), Atm(..))

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Int)

main :: IO ()
main = hspec $ do
  describe "P01 (*) Find the last element of a list" $ do
    describe "myLast" $ do
      it "returns the last element of an *any* list" $
        property $ \case
          l@[] -> isNothing (P99.myLast l)
          l    -> P99.myLast l == Just (last l :: Int)
    describe "Sx.myLast" $ do
      prop "returns the last element of an *any* uniform non-empty list" $
        \(h,t)  -> do let l = (h:t) :: [Int]
                          myL = Sx.fromList l
                      got <- Sx.myLast myL
                      got `shouldBe` Vx (last l)
      it "should throw an error for empty lists" $ do
        Sx.myLast (Sx.NIL :: Sx Int) `shouldThrow` errLike (bits ["Empty", "no last"])
      it "should throw an error for non-lists" $ do
        Sx.myLast (Vx True) `shouldThrow` errLike (bits ["Not", "a list"])
        Sx.myLast (Vx 'a' Sx.:~ Vx 'b') `shouldThrow` errLike (bits ["Not", "a list", "'b'"])
      it "should return the last element of non-homogenous lists" $
        let expected = (Vx 'b' Sx.:~ Vx 'c')
            l = (Vx 'a' Sx.:~ expected Sx.:~ Sx.NIL)
          in do got <- Sx.myLast l
                got `shouldBe` expected
    describe "Sxpr.myLast" $
      let b = Atm . B
          c = Atm . C
          s = Atm . S
      in do
      prop "returns the last element of an *any* uniform non-empty list" $
        \(h,t)  -> do let l = (h:t) :: [Integer]
                          myL = Sxpr.fromList l
                      got <- Sxpr.myLast myL
                      got `shouldBe` Atm (I (last l))
      it "should throw an error for empty lists" $ do
        Sxpr.myLast NIL `shouldThrow` errLike (bits ["Empty", "no last"])
      it "should throw an error for non-lists" $ do
        Sxpr.myLast (b True) `shouldThrow` errLike (bits ["Not", "a list"])
        Sxpr.myLast (c 'a' :~ c 'b') `shouldThrow` errLike (bits ["Not", "a list", "'b'"])
      it "should return the last element of non-homogenous lists" $
        let expected = (c 'b' :~ s "c")
            l = (c 'a' :~ expected :~ NIL)
          in do got <- Sxpr.myLast l
                got `shouldBe` expected


  describe "P02 (*) Find the last but one element of a list. " $ do
    describe "lastBut1" $ do
      prop "returns the second-last element of a big enough list" $
        \l -> (length l >= 2) ==> P99.lastBut1 (l:: [Int])
              `shouldBe` Just (head $ tail $ reverse l)
      prop "should return Nothing for a singleton list" $
        \a -> P99.lastBut1 [a::Int] `shouldBe` Nothing
      it "should return Nothing for an empty list" $ do
        P99.lastBut1 ([]::[Int]) `shouldBe` Nothing
    describe "Sx.lastBut1" $ do
      prop "returns the second-last element of a big enough list" $
        \l -> (length l >= 2) ==>
              do let myL    = Sx.fromList (l :: [Int])
                     expect = head $ tail $ reverse l
                 got <- Sx.lastBut1 myL
                 got `shouldBe` Vx expect
    describe "Sxpr.lastBut1" $ do
      prop "returns the second-last element of a big enough list" $
        \l -> (length l >= 2) ==>
              do let myL    = Sxpr.fromList (l :: [Integer])
                     expect = head $ tail $ reverse l
                 got <- Sxpr.lastBut1 myL
                 got `shouldBe` Atm (I expect)

  describe "P03 (*) Find the K'th element of a list. " $ do
    describe "elementAt" $ do
      prop "return the kth element of any list." $
        \l i -> P99.elementAt i (l::[Int]) `shouldBe`
                lookup i ([1..] `zip` l)
    describe "Sx.elementAt" $ do
      prop "return the kth element of any list." $
        \l i -> Sx.elementAt i (Sx.fromList (l::[Int])) `shouldBe`
                Vx <$> lookup i ([1..] `zip` l)
    describe "Sxpr.elementAt" $ do
      prop "return the kth element of any list." $
        \l i -> Sxpr.elementAt i (Sxpr.fromList (l::[Char])) `shouldBe`
                Atm . C <$> lookup i ([1..] `zip` l)

  describe "P04 (*) Find the number of elements of a list." $ do
    describe "numElements" $ do
      prop "must give you the length of the list" $
        \l -> P99.numElements l `shouldBe` length (l :: String)
    describe "Sx.numElements" $ do
      prop "must give you the length of the list" $
        \l -> Sx.numElements (Sx.fromList l) `shouldBe` Just (fromIntegral $ length (l :: String))
    describe "Sxpr.numElements" $ do
      prop "must give you the length of the list" $
        \l -> Sxpr.numElements (Sxpr.fromList l) `shouldBe`
              Just (fromIntegral $ length (l :: String))

  describe "P05 (*) Reverse a list." $ do
    describe "myReverse" $ do
      prop "must reverse any list (like reverse)" $
        \l -> P99.myReverse l `shouldBe` reverse (l :: [Double])
    describe "Sx.myReverse" $ do
      prop "must reverse any list (like reverse)" $
        \l -> Sx.myReverse (Sx.fromList l) `shouldBe`
              Sx.fromList <$> Just (reverse (l :: [Double]))
    describe "Sxpr.myReverse" $ do
      prop "must reverse any list (like reverse)" $
        \l -> Sxpr.myReverse (Sxpr.fromList l) `shouldBe`
              Sxpr.fromList <$> Just (reverse (l :: [Double]))

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
    describe "Sx.isPalindrome" $ do
      prop "any list appended to it's reverse should be seen as a palindrome" $
        \l -> let l' = Sx.fromList $ (l :: String) ++ reverse l in
              Sx.isPalindrome l' `shouldBe` Just True
      prop "any list appended to it's reverse sandwiching an element should be seen as a palindrome" $
        \l a -> let l' = Sx.fromList $ (l :: String) ++ [a] ++ reverse l in
              Sx.isPalindrome l' `shouldBe` Just True
      prop "any singleton list  should be seen as a palindrome" $
        \a -> Sx.isPalindrome (Vx (a::Int) Sx.:~ Sx.NIL) `shouldBe`Just  True
    describe "Sxpr.isPalindrome" $ do
      prop "any list appended to it's reverse should be seen as a palindrome" $
        \l -> let l' = Sxpr.fromList $ (l :: String) ++ reverse l in
              Sxpr.isPalindrome l' `shouldBe` Just True
      prop "any list appended to it's reverse sandwiching an element should be seen as a palindrome" $
        \l a -> let l' = Sxpr.fromList $ (l :: String) ++ [a] ++ reverse l in
              Sxpr.isPalindrome l' `shouldBe` Just True
      prop "any singleton list  should be seen as a palindrome" $
        \a -> Sxpr.isPalindrome (Atm (I (a::Integer)) :~ NIL) `shouldBe`Just True

  describe "P07 (**) Flatten a nested list structure. " $ do
    describe "(Sx) S-expressions" $
      let  n :: Int -> Sx Int
           n = Vx
           isList = Sx.isList
           append = Sx.append
           fromL = Sx.fromList
           flatten = Sx.flatten
      in do
      describe "show" $ do
        it "show should render lists correctly" $ do
          show (Sx.NIL :: Sx Int) `shouldBe` "()"
          show (n 1 Sx.:~ n 2 Sx.:~ n 4 Sx.:~ Sx.NIL) `shouldBe` "(1 2 4)"
          show (n 1 Sx.:~ (n 2 Sx.:~ Sx.NIL) Sx.:~ n 4 Sx.:~ Sx.NIL) `shouldBe` "(1 (2) 4)"
          show (n 3 Sx.:~ n 1) `shouldBe` "(3 . 1)"
        prop "show should render values correctly" $
          \i -> show (n i) `shouldBe` show i
      describe "isList" $ do
        it "should detect any valid list" $ do
          isList Sx.NIL `shouldBe` True
          isList (n 1 Sx.:~ Sx.NIL) `shouldBe` True
          isList ((n 1 Sx.:~ n 2) Sx.:~ Sx.NIL) `shouldBe` True
        it "should reject any non-list" $ do
          isList (n 7) `shouldBe` False
          isList (Vx True) `shouldBe` False
          isList (n 1 Sx.:~ n 2) `shouldBe` False
      describe "append" $ do
        it "should reject non-lists" $ do
          append (n 1) Sx.NIL `shouldThrow` errLike (bits ["Not a list", ": 1"])
          append Sx.NIL (n 2 Sx.:~ n 3) `shouldThrow` errLike (bits ["Not", "list", "(2 . 3)"])
        prop "should append lists" $
          \(l1, l2) -> do l3' <- append (fromL l1) (fromL l2)
                          l3' `shouldBe` fromL (l1 ++ l2)
      describe "flatten" $ do
        it "should reject non-lists" $ do
          flatten (n 1) `shouldThrow` errLike (bits ["non-list", ": 1"])
          flatten (n 2 Sx.:~ n 3) `shouldThrow` errLike (bits ["non", "list:3"])
        prop "should do nothing if given an already flat list" $
          \l -> do fl <- Sx.flatten (fromL (l :: [Int]))
                   fl `shouldBe` fromL l
        it "should flatten nested lists" $
          let l1 = (n 1 Sx.:~ fromL [23, 45] Sx.:~ ((n 7 Sx.:~ fromL [8, 11] Sx.:~ Sx.NIL) Sx.:~ n 13 Sx.:~ Sx.NIL) Sx.:~ Sx.NIL)
          in do
            show l1 `shouldBe` "(1 (23 45) ((7 (8 11)) 13))"
            fl1 <- flatten l1
            fl1 `shouldBe` fromL [1, 23, 45, 7, 8, 11, 13]
    describe "(Sxpr) S-expressions" $
      let  n :: Integer -> Sxpr
           n = Atm . I
           isList = Sxpr.isList
           append = Sxpr.append
           fromL = Sxpr.fromList
           flatten = Sxpr.flatten
      in do
      describe "show" $ do
        it "show should render lists correctly" $ do
          show NIL `shouldBe` "()"
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
          isList (Atm $ B True) `shouldBe` False
          isList (n 1 :~ n 2) `shouldBe` False
      describe "append" $ do
        it "should reject non-lists" $ do
          append (n 1) NIL `shouldThrow` errLike (bits ["Not a list", ": 1"])
          append NIL (n 2 :~ n 3) `shouldThrow` errLike (bits ["Not", "list", "(2 . 3)"])
        prop "should append lists" $
          \(l1, l2) -> do l3' <- append (fromL l1) (fromL l2)
                          l3' `shouldBe` fromL ((l1 ++ l2) :: [Integer])
      describe "flatten" $ do
        it "should reject non-lists" $ do
          flatten (n 1) `shouldThrow` errLike (bits ["non-list", ": 1"])
          flatten (n 2 :~ n 3) `shouldThrow` errLike (bits ["non", "list:3"])
        prop "should do nothing if given an already flat list" $
          \l -> do fl <- flatten (fromL (l :: [Integer]))
                   fl `shouldBe` fromL l
        it "should flatten nested lists" $
          let l1 = (n 1 :~ fromL [23, 45] :~ ((n 7 :~ fromL [8, 11] :~ NIL) :~ n 13 :~ NIL) :~ NIL)
          in do
            show l1 `shouldBe` "(1 (23 45) ((7 (8 11)) 13))"
            fl1 <- flatten l1
            fl1 `shouldBe` fromL [1, 23, 45, 7, 8, 11, 13]

  describe "P08 (**) Eliminate consecutive duplicates of list elements. " $ do
    describe "compress" $ do
      it "should behave like `uniq` (UNIX)" $ do
        P99.compress [] `shouldBe` ([] :: [Int])
        P99.compress "aabbbacbd" `shouldBe` "abacbd"
    describe "Sxpr.compress" $
      let fromL    = Sxpr.fromList
          compress = Sxpr.compress
          n        = Atm . I
      in do
      it "should behave like `uniq` (UNIX)" $ do
        compress NIL `shouldBe` NIL
        compress (fromL "aabbbacbd") `shouldBe` (fromL "abacbd")
        compress (n 1 :~ n 1 :~ NIL) `shouldBe` (n 1 :~ NIL)
      it "should leave tuples and atoms alone" $ do
        compress (n 1 :~ n 1) `shouldBe` (n 1 :~ n 1)
        compress (n 42) `shouldBe` n 42

  describe "P09 (**) Pack consecutive duplicates of list elements into sublists. " $ do
    describe "pack" $ do
      prop "should behave like Data.List (group)" $ do
        \l -> P99.pack l `shouldBe` group (l :: [Char])
    describe "Sxpr.pack" $
      let fromL = foldr (:~) NIL
      in do
      prop "should behave like Data.List (group)" $ verbose $ do
        \l -> do
          expected <- return $ fromL $ Sxpr.fromList <$> group (l :: [Integer])
          got      <- Sxpr.pack (Sxpr.fromList l)
          got `shouldBe` expected

-- UTILS
bits :: [String] -> String -> Bool
bits ss got = all (`isSubsequenceOf` got) ss

errLike :: (String -> Bool) -> SomeException -> Bool
errLike msgMatch (SomeException msg) = msgMatch $ show msg
