import Data.Maybe

-- Stack DS
newtype Stack a = Stack [a] deriving Show
emptySt :: Stack a
emptySt = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

-- Tree DS
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Pre-order traversal
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)

-- In-order traversal
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

-- Post-order traversal
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- DFS
traverseDFS :: Tree a -> [a]
traverseDFS Empty = []
traverseDFS (Node a l r) = a : (traverseDFS l) ++ (traverseDFS r)

-- BFS
traverseBFS :: Tree a -> [a]
traverseBFS tree = tbf [tree]
  where
    tbf [] = []
    tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
    
    nodeValue (Node a _ _) = a
    
    leftAndRightNodes (Node _ Empty Empty) = []
    leftAndRightNodes (Node _ Empty b) = [b]
    leftAndRightNodes (Node _ a Empty) = [a]
    leftAndRightNodes (Node _ a b) = [a,b]
  
  
createTree = Node 'A'
                (Node 'B'
                    (Node 'C' Empty Empty)
                    (Node 'D' Empty Empty)
                )
                (Node 'E'
                    (Node 'F' Empty Empty)
                    (Node 'G' Empty (Node 'H'
                        (Node 'I' Empty Empty)
                        Empty
                    ))
                )
                
                
-- Sorting algorithms

-- Quicksort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

-- Mergesort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs
  | (length xs) > 1 = mergeSort'merge (mergeSort ls) (mergeSort rs)
  | otherwise = xs
  where (ls, rs) = mergeSort'split xs

mergeSort'split :: (Ord a) => [a] -> ([a],[a])
mergeSort'split xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

mergeSort'merge :: (Ord a) => [a] -> [a] -> [a]
mergeSort'merge [] ys = ys
mergeSort'merge xs [] = xs
mergeSort'merge (x:xs) (y:ys)
  | (x < y) = x : mergeSort'merge xs (y:ys)
  | otherwise = y : mergeSort'merge (x:xs) ys


