{-# LANGUAGE LambdaCase #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Data.Maybe (isNothing)

import qualified P99.Lists as P99

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
