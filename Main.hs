module Main where

import Menu (menu)
import Tree
  ( BinaryTree,
    inOrder,
    insertOnTree,
    insertSeveral,
    postOrder,
    preOrder,
    printTree,
    tree,
    treeDelete,
    treeHeight,
    isBalanced,
  )

main :: IO ()
main = do
  menu
  temp <- getLine
  case temp of
    "1" -> printTree tree
    "2" -> do
      putStrLn "Elemento a ser inserido:"
      value <- getLine
      putStrLn "\n"
      printTree (insertOnTree value tree)
    "3" -> do
      putStrLn "Elemento a ser deletado:"
      value <- getLine
      putStrLn "\n"
      printTree (treeDelete value tree)
    "4" -> print (inOrder tree)
    "5" -> print (preOrder tree)
    "6" -> print (postOrder tree)
    "7" -> print (treeHeight tree)
    "8" -> print (isBalanced tree)
  main