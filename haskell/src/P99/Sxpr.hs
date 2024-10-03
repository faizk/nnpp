{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module P99.Sxpr
    ( Sxpr(..)
    , Atm(..)
    , isList
    , append
    , fromList
    -- redoing List problems
    , myLast
    , lastBut1
    , elementAt
    , numElements
    , myReverse
    , isPalindrome
    , flatten
    , compress
    , pack
    ) where

import Control.Monad (join, (<=<))
import Numeric.Natural (Natural)

data Atm where
  S :: String -> Atm
  C :: Char -> Atm
  B :: Bool -> Atm
  -- N :: Num a => a -> Atm -- now if only..
  I :: Integer -> Atm
  D :: Double -> Atm

deriving instance Eq Atm
instance Show Atm where
  show = \case
    S s -> show s
    C c -> show c
    B b -> show b
    I i -> show i
    D d -> show d

class CanBeAtm a where atm :: a -> Atm

instance CanBeAtm Integer where atm = I
instance CanBeAtm String  where atm = S
instance CanBeAtm Char    where atm = C
instance CanBeAtm Bool    where atm = B
instance CanBeAtm Double  where atm = D

data Sxpr
  = Atm Atm
  | Sxpr :~ Sxpr
  | NIL
  deriving Eq

infixr 5 :~

instance Show Sxpr where
  show (Atm a) = show a
  show NIL = "()"
  show (car :~ cdr) | isList cdr =
    "(" ++ show car ++ f cdr
      where
        f NIL        = ")"
        f (h :~ t)   = " " ++ show h ++ f t
        f x          = " . " ++ show x ++ ")"
  show (car :~ cdr)  = "(" ++ show car ++ " . " ++ show cdr ++ ")"

isList :: Sxpr -> Bool
isList = \case
  _ :~ t -> isList t
  NIL   -> True
  Atm _ -> False

append :: MonadFail m => Sxpr -> Sxpr -> m Sxpr
append sxa NIL | isList sxa = pure sxa
append sxa NIL                = fail $ "Not a list: " ++ show sxa
append NIL sxb | isList sxb = pure sxb
append NIL sxb                = fail $ "Not a list: " ++ show sxb
append (h :~ t) lb            = (h :~) <$> append t lb
append sxa _                  = fail $ "Not a list: " ++ show sxa

fromList :: CanBeAtm a =>[a] -> Sxpr
fromList = foldr ((:~) . Atm . atm) NIL

-- redoing some of the "List" problems so as to base it on this type

-- P01 (*) Find the last element of a list.
myLast :: MonadFail m => Sxpr -> m Sxpr
myLast (h :~ NIL) = pure h
myLast (_ :~ t)   = myLast t
myLast NIL        = fail "Empty list, no last element"
myLast sxp        = fail $ "Not a list: " ++ show sxp

-- P02 (*) Find the last but one element of a list.
lastBut1 :: MonadFail m => Sxpr -> m Sxpr
lastBut1 (x :~ _ :~ NIL)  = pure x
lastBut1 small@(_ :~ NIL) = fail $ "only one element: " ++ show small
lastBut1 (_ :~ t)         = lastBut1 t
lastBut1 NIL              = fail "Empty list, no last element"
lastBut1 sxp              = fail $ "Not a list: " ++ show sxp

-- P03 (*) Find the K'th element of a list.
elementAt :: MonadFail m => Natural -> Sxpr -> m Sxpr
elementAt 1 (h :~ t) | isList t = return h
elementAt i _        | i < 1    = fail $ "invalid index: " ++ show i
elementAt i (_ :~ t)            = elementAt (i-1) t
elementAt _ NIL                 = fail "Empty list"
elementAt _ sxp                 = fail $ "Not a list: " ++ show sxp

-- P04 (*) Find the number of elements of a list.
numElements :: MonadFail m => Sxpr -> m Integer
numElements (_ :~ t)            = (1+) <$> numElements t
numElements NIL                 = pure 0
numElements sxp                 = fail $ "Not a list: " ++ show sxp

-- P05 (*) Reverse a list.
myReverse :: MonadFail m => Sxpr -> m Sxpr
myReverse = rev NIL
  where rev acc (h :~ t) = rev (h :~ acc) t
        rev acc NIL      = pure acc
        rev _   sxp      = fail $ "Not a list :" ++ show sxp

-- P06 (*) Find out whether a list is a palindrome.
isPalindrome :: MonadFail m => Sxpr -> m Bool
isPalindrome lst = (lst ==) <$> myReverse lst

flatten :: MonadFail m => Sxpr -> m Sxpr
flatten (car :~ cdr) | isList car = join $ append <$> flatten car <*> flatten cdr
flatten (car :~ cdr) = (car :~) <$> flatten cdr
flatten NIL = pure NIL
flatten x = fail $ "Can't flatten a non-list: " ++ show x

compress :: Sxpr -> Sxpr
compress (a :~ b :~ rest) | a == b = compress (b :~ rest)
compress (a :~ rest)               = a :~ compress rest
compress whatever                  = whatever

pack :: MonadFail m => Sxpr -> m Sxpr
pack = myReverse <=< p NIL
  where
    p acc NIL = return acc
    p (accH@(h' :~ _) :~ accT) (h :~ t) | h == h' = p ((h :~ accH) :~ accT) t
    p acc                      (h :~ t)           = p ((h :~ NIL) :~ acc)   t
    p _                        blah               =
      fail $ "Can't pack a non-list: " ++ show blah
