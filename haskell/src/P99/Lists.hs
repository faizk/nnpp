module P99.Lists
    ( myLast
    ) where

myLast :: [a] -> Maybe a
myLast [a] = Just a
myLast [] = Nothing
myLast (_:rest) = myLast rest
