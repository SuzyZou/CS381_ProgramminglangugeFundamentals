module HW2types where

import Data.List 


-- Types for Exercise 1
--
type Bag a = [(a,Int)]


-- Types and functions for Exercise 2
--
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub


-- Types for Exercise 3
--
type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)
