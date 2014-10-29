module Functors where

data Barry t k p = Barry { yabba :: p, dabba :: t k }  

instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}  

-- BOOOOM, head explosion
-- fmap (++ "!") getLine
-- fmap (fmap (*6) (subtract 1)) $ Just 4
-- fmap ((*6) . (subtract 1)) $ Just 4



data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (succ counter) (f x)  