module Util where

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (x:xs) = case f x of
  Just y -> Just y
  Nothing -> findMaybe f xs

(<?>) :: Maybe a -> Maybe a -> Maybe a
Just x <?> _ = Just x
_ <?> Just y = Just y
_ <?> _ = Nothing

infixr 1 ??
(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y

