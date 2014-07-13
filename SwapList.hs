{-
  Given two positions in a list, swap them. If (at least) one of the positions is invalid, return Nothing.

  Examples:

  swapL 0 2 [0..5]
  Just [2, 1, 0, 3, 4, 5]

  fmap (take 6) $ swapL 0 2 [0..]
  Just [2, 1, 0, 3, 4, 5]

  prop> \(x, y) -> swapL x y [] == Nothing
  prop> \(n, xs) -> swapL n n xs == Just xs
-} 

import Data.List
import Test.QuickCheck

swapL:: Int -> Int -> [a] -> Maybe [a]
swapL x y [] = Nothing
swapL 0 0 [z] = Just [z]
swapL x y [z] = Nothing
swapL x y xs
  | x < 0 || y < 0 = Nothing
  | x > y = swapL y x xs
  | otherwise = if null xxs || null yys then Nothing
                else Just $ px ++ [(head yys)] ++ (tail py) ++ [(head xxs)] ++ (tail yys)
  where                  
    (px, xxs) = splitAt (pred x) xs
    (py, yys) = splitAt y xxs

prop_empty :: Int -> Int -> Bool
prop_empty x y = swapL x y ([] :: [Int]) == Nothing

prop_idx_eq :: Int -> [Char] -> Bool
prop_idx_eq x [] = True
prop_idx_eq x xs = swapL x x xs == Just xs

main = do
  quickCheck prop_empty
  quickCheck prop_idx_eq
