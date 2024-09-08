module P99.Lists
    ( myLast
    , lastBut1
    , elementAt
    , numElements
    , myReverse
    , isPalindrome
    , flatten
    ) where

import Numeric.Natural

import qualified P99.Sx as Sx
import P99.Sx (Sx(..))
import Control.Monad (join)

myLast :: [a] -> Maybe a
myLast [a] = Just a
myLast [] = Nothing
myLast (_:rest) = myLast rest

lastBut1 :: [a] -> Maybe a
lastBut1 [a, _] = Just a
lastBut1 [] = Nothing
lastBut1 (_:rest) = lastBut1 rest

elementAt :: Natural -> [a] -> Maybe a
elementAt 1 (x:_) = Just x
elementAt i (_:l) | i > 1 = elementAt (i-1) l
elementAt _ _ = Nothing

numElements :: [a] -> Int
numElements = num1 0 where
  num1 c [] = c
  num1 c (_:rest) = num1 (c + 1) rest

myReverse :: [a] -> [a]
myReverse = rev [] where rev soFar []     = soFar
                         rev soFar (x:xs) = rev (x:soFar) xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == myReverse l

flatten :: (Show a, MonadFail m) => Sx a -> m (Sx a)
flatten (car :~ cdr) | Sx.isList car = join $ Sx.append <$> flatten car <*> flatten cdr
flatten (car :~ cdr) = (car :~) <$> flatten cdr
flatten Sx.NIL = pure Sx.NIL
flatten x = fail $ "Can't flatten a non-list: " ++ show x
