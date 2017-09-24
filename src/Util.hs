module Util where

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (x:xs) = case f x of
  Just y -> Just y
  Nothing -> findMaybe f xs

