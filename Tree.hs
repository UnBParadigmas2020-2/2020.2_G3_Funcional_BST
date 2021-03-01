module Tree
  ( BinaryTree,
    tree,
    printTree,
    inOrder,
    insertSeveral,
    insertOnTree,
    preOrder,
    postOrder,
    treeDelete,
    treeHeight,
    isBalanced,
    stackValues,
    deleteSeveral,
    searchInTree,
  )
where

import Data.Tree (Tree (Node))
import Data.Tree.Pretty (drawVerticalTree)

data BinaryTree a = Null | No a (BinaryTree a, BinaryTree a)
  deriving (Eq, Read, Ord, Show)

toDataTree :: BinaryTree a -> Data.Tree.Tree a
toDataTree (No x (Null, Null)) = Node x []
toDataTree (No x (left, Null)) = Node x [toDataTree left]
toDataTree (No x (Null, right)) = Node x [toDataTree right]
toDataTree (No x (left, right)) = Node x [toDataTree left, toDataTree right]

tree :: BinaryTree [Char]
tree = No "m" (No "g" (No "d" (Null, Null), No "j" (Null, Null)), No "t" (No "p" (Null, Null), No "w" (Null, Null)))

printTree :: BinaryTree [Char] -> IO ()
printTree x = putStrLn $ drawVerticalTree (toDataTree x)

insertOnTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertOnTree x Null = No x (Null, Null)
insertOnTree x (No y (left, right))
  | x == y = No y (left, right)
  | x < y = No y (insertOnTree x left, right)
  | x > y = No y (left, insertOnTree x right)

insertSeveral :: (Ord a) => [a] -> BinaryTree a -> BinaryTree a
insertSeveral x (No y (left, right)) = foldr insertOnTree (No y (left, right)) $ reverse x

searchInTree :: (Ord a) => a -> BinaryTree a -> Bool
searchInTree _ Null = False
searchInTree y (No x (left, right))
  | x == y = True
  | y < x = searchInTree y left
  | otherwise = searchInTree y right

treeDelete :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeDelete x Null = Null
treeDelete x (No y (left, right))
  | x < y = No y (treeDelete x left, right)
  | x > y = No y (left, treeDelete x right)
  | left == Null = right
  | right == Null = left
  | otherwise = No newItem (newLeft, right)
  where
    newItem = treeMax left
    newLeft = treeDelete newItem left

deleteSeveral :: (Ord a) => [a] -> BinaryTree a -> BinaryTree a
deleteSeveral x (No y (left, right)) = foldr treeDelete (No y (left, right)) $ reverse x

treeHeight :: (Num p, Ord p) => BinaryTree a -> p
treeHeight Null = 0
treeHeight (No x (left, right)) = 1 + max (treeHeight left) (treeHeight right)

isBalanced :: BinaryTree a -> Bool
isBalanced Null = True
isBalanced (No x (left, right)) = abs (treeHeight left - treeHeight right) <= 1 && isBalanced left && isBalanced right

inOrder :: BinaryTree a -> [a]
inOrder Null = []
inOrder (No x (left, right)) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Null = []
preOrder (No x (left, right)) = [x] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Null = []
postOrder (No x (left, right)) = postOrder left ++ postOrder right ++ [x]

-- Auxiliary Functions

stackValues :: IO [String]
stackValues = do
  input <- getLine
  if null input
    then return []
    else do
      moreInputs <- stackValues
      return (input : moreInputs)

treeMax :: (Eq a) => BinaryTree a -> a
treeMax (No x (_, right)) =
  if right /= Null
    then treeMax right
    else x

-- Unused

{-saveTree :: (Show a) => BinaryTree a -> IO ()
saveTree x = do
  output <- openFile "tree.txt" WriteMode
  hPrint output (inOrder x)
  hPrint output (preOrder x)
  hClose output

retrieveLists :: IO ([Char], [Char])
retrieveLists = do
  s <- readFile "tree.txt"
  let a = read (take ((length (filterString s) `div` 2) - 1) (filterString s))
  let b = read (take ((length (filterString s) `div` 2) - 1) (drop (length (filterString s) `div` 2) (filterString s)))
  return (a, b)

filterString :: [Char] -> [Char]
filterString = filter (not . (`elem` ".?!-:;\"\'"))-}