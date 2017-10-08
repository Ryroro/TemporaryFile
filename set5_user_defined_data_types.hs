
data Shape = Triangle Float Float Float
           | Square Float
           | Circle Float
           deriving (Show)

area :: Shape -> Float
area (Triangle a b c) = sqrt (s*(s-a)*(s-b)*(s-c))
                    where s = (a + b + c) / 2

area (Square a) = a * a

area (Circle r) = pi * r * r

type Date = (Int, Int, Int)

age :: Date -> Date -> Int
age (y1, m1, d1) (y2, m2, d2) = y2 - y1

data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
-- Return the root of the new Tree
singleton x = Node EmptyTree x EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
-- Insert a value into a tree
treeInsert x EmptyTree = singleton x
treeInsert x (Node left a right)
  | x == a = Node left x right
  | x < a  = Node (treeInsert x left) a right
  | x > a  = Node left a (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
-- Check if the given value is contained by a node in the tree
treeElem x EmptyTree = False
treeElem x (Node left a right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

flatten1 :: Tree a -> [a]
flatten1 EmptyTree = []
flatten1 (Node t1 x t2) = flatten t1 ++ (x : flatten t2)

flatten :: Tree a -> [a]
-- Run through the tree in a in-order way
-- Note : in-order means search the left tree first, then node value,
--        then finally the right tree
flatten (Node left x right) =  flatten' (Node left x right) []
                            where flatten' EmptyTree acc = acc
                                  flatten' (Node left x right) acc = flatten' left ( x : flatten' right acc)

data Tree5 = Leaf | Node5 Tree5 Tree5
           deriving (Eq, Show)
-- Note : the 5 denote that this data structure is from question number 5

--makeTrees :: Int -> [Tree]
-- given an integer n, will return a list of all binary trees with n nodes


