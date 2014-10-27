module Curry (multThree, dupFunc, largestDisvisiable, maximum', oddSquareSum) where


multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 

dupFunc :: (a -> a) -> a -> a
dupFunc f x = f $ f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _  = []
zipWith' _ _  [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

largestDisvisiable :: (Integral a) => a -> a
largestDisvisiable n = head (filter p [n,n-1..])
  where p x = x `mod` 3829 == 0

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  

oddSquareSum :: Integer  
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))     
--oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2)  [1..]

