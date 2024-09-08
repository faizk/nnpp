{-# LANGUAGE LambdaCase #-}
module P99.Sx
    ( Sx(..)
    , isList
    , append
    ) where

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

