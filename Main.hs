module Main where

import Menu (menu)
import System.Exit (exitSuccess)
import Tree
  ( BinaryTree,
    deleteSeveral,
    inOrder,
    insertOnTree,
    insertSeveral,
    isBalanced,
    postOrder,
    preOrder,
    printTree,
    stackValues,
    tree,
    treeDelete,
    treeHeight,
    searchInTree,
  )

main :: IO ()
main = do
  menu
  temp <- getLine
  putStr "\n"
  case temp of
    "1" -> printTree tree
    "2" -> do
      putStr "Elemento a ser inserido: "
      value <- getLine
      putStr "\n"
      printTree (insertOnTree value tree)
    "3" -> do
      putStr "Elementos a serem inseridos\n(um elemento por linha, linha em branco para encerrar inserção): "
      value <- stackValues
      putStr "\n"
      printTree (insertSeveral value tree)
    "4" -> do
      putStr "Elemento a ser deletado: "
      value <- getLine
      putStr "\n"
      printTree (treeDelete value tree)
    "5" -> do
      putStr "Elementos a serem deletados\n(um elemento por linha, linha em branco para encerrar inserção): "
      value <- stackValues
      putStr "\n"
      printTree (deleteSeveral value tree)
    "6" -> do
      putStr "Elemento a ser procurado: "
      value <- getLine
      putStr "\n"
      print (searchInTree value tree)
      putStr "\n"
    "7" -> do
      print (inOrder tree)
      putStr "\n"
    "8" -> do
      print (preOrder tree)
      putStr "\n"
    "9" -> do
      print (postOrder tree)
      putStr "\n"
    "10" -> do
      print (treeHeight tree)
      putStr "\n"
    "11" -> do
      print (isBalanced tree)
      putStr "\n"
    "12" -> exitSuccess
    _ -> putStrLn "Entrada inválida.\n"
  main