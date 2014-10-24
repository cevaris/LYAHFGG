module Functions where


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
-- Point Manipulation
--data Point =  Point2 Integer Integer 
--            | Point3 Integer Integer Integer

type Point = (Integer, Integer)

--midpoint :: (Point a) => a -> a -> a
--midpoint (Point2 x1 y1) (Point2 x2 y2)       = (Point2 2 2)
--midpoint (Point3 x1 y1 z1) (Point3 x2 y2 z2) = (Point3 2 2 3)

midpoint :: Point -> Point -> Point
midpoint (x1,y1) (x2, y2) = (((x1+x2) `div` 2), ((y1+y2) `div` 2))