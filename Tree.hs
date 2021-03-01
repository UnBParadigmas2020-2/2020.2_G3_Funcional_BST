module Tree where

import Data.Tree (Tree (Node))
import Data.Tree.Pretty (drawVerticalTree)
import System.IO (IOMode (WriteMode), hClose, hPrint, openFile)

data BinaryTree a = Null | No a (BinaryTree a, BinaryTree a)
  deriving (Eq, Read, Ord, Show)

toDataTree :: BinaryTree a -> Data.Tree.Tree a
toDataTree (No a (Null, Null)) = Node a []
toDataTree (No a (left, Null)) = Node a [toDataTree left]
toDataTree (No a (Null, right)) = Node a [toDataTree right]
toDataTree (No a (left, right)) = Node a [toDataTree left, toDataTree right]

tree :: BinaryTree [Char]
tree = No "k" (No "f" (No "d" (No "c" (Null, Null), No "e" (Null, Null)), No "h" (Null, Null)), No "o" (Null, Null))

printTree :: BinaryTree [Char] -> IO ()
printTree x = putStrLn $ drawVerticalTree (toDataTree x)

insertOnTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertOnTree x Null = No x (Null, Null)
insertOnTree x (No a (left, right))
  | x == a = No a (left, right)
  | x < a = No a (insertOnTree x left, right)
  | x > a = No a (left, insertOnTree x right)

inTree :: Ord a => BinaryTree a -> a -> Bool
inTree Empty _ = False
inTree (No left x right) y = if x == y then True
	else
		if y < x then inTree left y
			else inTree right y


delete :: Ord a => BinaryTree a -> a -> BinaryTree a
delete t@(No left x right) y = if inTree t y 
	then
		if y < x 
		then (No (delete left y) x right)
		else 
			if y > x
			then (No left x (delete right y))
			else deleteNode t
	else t

deleteNode :: Ord a => BinaryTree a -> BinaryTree a
deleteNode (No left x right) = if left == Empty 
	then right
	else
		if right == Empty
		then left
		else (No left (leftMost right) (update right))

leftMost :: Ord a => BinaryTree a -> a
leftMost (No Empty x _) = x
leftMost (No left x _) = leftMost left

update :: Ord a => BinaryTree a -> BinaryTree a
update Empty = Empty
update t@(No left x right) =  if x == (leftMost t)
	then delete t x
	else 
		if x < (leftMost t) 
		then No left x (update right)
			else No (update left) x right

treeHeight :: (Num p, Ord p) => BinaryTree a -> p
treeHeight Null = 0
treeHeight (No x (left, right)) = 1 + max (treeHeight left) (treeHeight right)

inOrder :: BinaryTree a -> [a]
inOrder Null = []
inOrder (No a (left, right)) = inOrder left ++ [a] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Null = []
preOrder (No a (left, right)) = [a] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Null = []
postOrder (No a (left, right)) = postOrder left ++ postOrder right ++ [a]

saveTree :: IO ()
saveTree = do
  output <- openFile "tree.txt" WriteMode
  hPrint output (inOrder tree)
  hPrint output (preOrder tree)
  hClose output