module HW2sol where 
import HW2types
sname = "Sean Harrington"
--Email: harrinse@oregonstate.edu
--Date: 1/26/2023


--Bags
ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a,n)]
ins a ((x,y):b)
    | a == x = ((x,y + 1) : b)
    | otherwise = ((x,y) : ins a)
 

del :: Eq a => a -> Bag a -> Bag 
del x = Bag . Map.update (deleteN 1) + x n . unMS

bag :: Eq a => [a] -> Bag a
bag [] == []
bag ((x,n):ys)
    | n == 1 = x:(bag ys)
    | otherwise = x:(bag (x,(n-1):ys)) 

subbag :: Eq a => Bag a -> Bag a -> Bool

isSet :: Eq a => Bag a -> Bool


size :: Bag a -> Int
size [] = 0
size [x] = 1
size (_:xs) = 1 + size(xs)

--Graphs

nodes :: Graph -> [Node]

suc :: Node -> Graph -> [Node]

detach :: Node -> Graph -> Graph

cyc :: Int -> Graph 

--Shapes

width :: Shape -> Length

bbox :: Shape -> BBox

minX :: Shape -> Number

move :: Shape -> Point -> Shape