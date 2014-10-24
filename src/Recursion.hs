module Recursion where

data Error = InvalidArgumentError String
           | InvalidPointArgument String
           | PointTypeNotSupported String
           deriving (Show, Eq)



--maximum' :: (Ord a) => [a] -> Either Error a
--maximum' []  = Left $ InvalidArgumentError "Empty List Found"
--maximum' [x] = Right x  
--maximum' (x:xs)   
--    | x > maxTail = Right x  
--    | otherwise = maxTail  
--    where maxTail = maximum' xs  


--maximum' :: (Ord a) => [a] -> a  
--maximum' [] = error "maximum of empty list"  
--maximum' [x] = x  
--maximum' (x:xs)   
--    | x > maxTail = x  
--    | otherwise = maxTail  
--    where maxTail = maximum' xs  


maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x $ maximum' xs

--replicate' :: Integer -> a -> [a]
--replicate' 0 z = []
--replicate' x z = [z] ++ replicate' (x-1) z

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _ | n <= 0  = []
take' _ []          = []
take' n (x:xs)      = x:take' (n-1) xs