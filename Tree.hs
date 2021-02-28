module Tree where

import Data.Tree (Tree (Node))
import Data.Tree.Pretty (drawVerticalTree)

data BinaryTree a = Null | No a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Ord, Show)

toDataTree :: BinaryTree a -> Data.Tree.Tree a
toDataTree (No a Null Null) = Node a []
toDataTree (No a left Null) = Node a [toDataTree left]
toDataTree (No a Null right) = Node a [toDataTree right]
toDataTree (No a left right) = Node a [toDataTree left, toDataTree right]

tempTree :: BinaryTree [Char]
tempTree = No "1" (No "11" (No "111" Null Null) (No "112" (No "190" Null Null) Null)) (No "12" (No "121" Null (No "151" Null Null)) (No "122" Null Null))

treeFormat :: Tree [Char]
treeFormat = toDataTree tempTree

printTree :: IO ()
printTree = putStrLn $ drawVerticalTree treeFormat