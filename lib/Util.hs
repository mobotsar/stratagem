module Util where

infixl 5 >.

(>.) :: (a -> b) -> (b -> c) -> a -> c
(>.) = flip (.)

infixl 4 |>

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

unwrapOr :: a -> Maybe a -> a
unwrapOr _ (Just x) = x
unwrapOr y _ = y

unwrap :: Maybe a -> a
unwrap = unwrapOr (error "Tried to unwrap Nothing.")

tup :: a -> b -> (a, b)
tup a b = (a, b)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
