module P99.Lists
    ( myLast
    , lastBut1
    , elementAt
    ) where
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
