module Functions (sayMe, midpoint, slope) where

import Control.Applicative

--------------------------------------------------------
-- Pattern Matching
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"


--------------------------------------------------------
--Point Manipulation
data Point = Point2 Integer Integer 
           | Point3 Integer Integer Integer 
           deriving (Show, Eq)

data Error = DivdeByZeroError
           | InvalidPointArgument
           | PointTypeNotSupported
           deriving (Show, Eq)

midpoint :: Point -> Point -> Maybe Point
midpoint (Point2 x1 y1) (Point2 x2 y2)       = Just (Point2 ((x1+x2) `div` 2) ((y1+y2) `div` 2))
midpoint (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Just (Point3 z1 y1 x1)

slope :: Point -> Point -> Either Error Integer
slope (Point2 x1 y1) (Point2 x2 y2) 
  | (x2-x1) == 0 = Left DivdeByZeroError
  | (x2-x1) /= 0 = Right $ (y2-y1) `div` (x2-x1)
slope (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Left PointTypeNotSupported