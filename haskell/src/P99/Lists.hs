module P99.Lists
    ( myLast
    , lastBut1
    , elementAt
    , numElements
    , myReverse
    , isPalindrome
    , compress
    , pack
    , encode
    ) where

import Control.Monad (join)
import Numeric.Natural

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

-- P08 (**) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (a:b:rest) | a == b = compress (b:rest)
compress (a:rest)          = a : compress rest

-- P09 (**) Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack = reverse . p []
  where
    p acc []  = acc
    p (accH@(h':_):accRest) (h:rest) | h == h' = p ((h:accH):accRest) rest
    p acc                   (h:rest)           = p ([h]:acc)          rest

-- P10 (*) Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode = join . map cnt . pack where
  cnt l@(a:_) = [(length l, a)]
  cnt []      = []
