{-# LANGUAGE LambdaCase #-}
module P99.Sx
    ( Sx(..)
    , isList
    , append
    , fromList
    -- redoing List problems
    , myLast
    , lastBut1
    , elementAt
    , numElements
    ) where

import Numeric.Natural (Natural)

data Sx a
  = Vx a
  | Sx a :~ Sx a
  | NIL
  deriving Eq

infixr 5 :~

instance Show a => Show (Sx a) where
  show (Vx a) = show a
  show NIL = "()"
  show (car :~ cdr) | isList cdr =
    "(" ++ show car ++ f cdr
      where
        f NIL        = ")"
        f (h :~ t)   = " " ++ show h ++ f t
        f x          = " . " ++ show x ++ ")"
  show (car :~ cdr)  = "(" ++ show car ++ " . " ++ show cdr ++ ")"

isList :: Sx a -> Bool
isList = \case
  _ :~ t -> isList t
  NIL    -> True
  Vx _   -> False

append :: (Show a, MonadFail m) => Sx a -> Sx a -> m (Sx a)
append sxa NIL | isList sxa = pure sxa
append sxa NIL                = fail $ "Not a list: " ++ show sxa
append NIL sxb | isList sxb = pure sxb
append NIL sxb                = fail $ "Not a list: " ++ show sxb
append (h :~ t) lb            = (h :~) <$> append t lb
append sxa _                  = fail $ "Not a list: " ++ show sxa

fromList :: [a] -> Sx a
fromList = foldr ((:~) . Vx) NIL

-- redoing some of the "List" problems so as to base it on this type

-- P01 (*) Find the last element of a list.
myLast :: (Show a, MonadFail m) => Sx a -> m (Sx a)
myLast (h :~ NIL) = pure h
myLast (_ :~ t)   = myLast t
myLast NIL        = fail "Empty list, no last element"
myLast sxp        = fail $ "Not a list: " ++ show sxp

-- P02 (*) Find the last but one element of a list.
lastBut1 :: (Show a, MonadFail m) => Sx a -> m (Sx a)
lastBut1 (x :~ _ :~ NIL)  = pure x
lastBut1 small@(_ :~ NIL) = fail $ "only one element: " ++ show small
lastBut1 (_ :~ t)         = lastBut1 t
lastBut1 NIL              = fail "Empty list, no last element"
lastBut1 sxp              = fail $ "Not a list: " ++ show sxp

-- P03 (*) Find the K'th element of a list.
elementAt :: (Show a, MonadFail m) => Natural -> Sx a -> m (Sx a)
elementAt 1 (h :~ t) | isList t = return h
elementAt i _        | i < 1    = fail $ "invalid index: " ++ show i
elementAt i (_ :~ t)            = elementAt (i-1) t
elementAt _ NIL                 = fail "Empty list"
elementAt _ sxp                 = fail $ "Not a list: " ++ show sxp

-- P04 (*) Find the number of elements of a list.
numElements :: (Show a, MonadFail m) => Sx a -> m Integer
numElements (_ :~ t)            = (1+) <$> numElements t
numElements NIL                 = pure 0
numElements sxp                 = fail $ "Not a list: " ++ show sxp
