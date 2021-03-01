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