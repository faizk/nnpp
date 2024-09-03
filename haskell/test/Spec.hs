{-# LANGUAGE LambdaCase #-}

import Test.Hspec
import Test.QuickCheck

import Data.Maybe (isNothing)

import qualified P99.Lists as P99

main :: IO ()
main = hspec $ do
  describe "P01 (*) Find the last element of a list" $ do

    it "returns the last element of an *any* list" $
      property $ \case
        l@[] -> isNothing (P99.myLast l)
        l    -> P99.myLast l == Just (last l :: Int)

