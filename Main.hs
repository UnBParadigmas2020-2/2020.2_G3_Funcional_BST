module Main where

import Menu (menu)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
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
    searchInTree,
    stackValues,
    tree,
    deleteNode,
    treeHeight,
  )

main :: IO ()
main = do
  menu
  hFlush stdout
  temp <- getLine
  putStrLn ""
  case temp of
    "1" -> printTree tree
    "2" -> do
      putStr "Elemento a ser inserido: "
      hFlush stdout
      value <- getLine
      putStrLn ""
      printTree (insertOnTree value tree)
    "3" -> do
      putStr "Elementos a serem inseridos\n(um elemento por linha, linha em branco para encerrar inserção): "
      hFlush stdout
      value <- stackValues
      printTree (insertSeveral value tree)
    "4" -> do
      putStr "Elemento a ser deletado: "
      hFlush stdout
      value <- getLine
      putStrLn ""
      printTree (deleteNode value tree)
    "5" -> do
      putStr "Elementos a serem deletados\n(um elemento por linha, linha em branco para encerrar inserção): "
      hFlush stdout
      value <- stackValues
      printTree (deleteSeveral value tree)
    "6" -> do
      putStr "Elemento a ser procurado: "
      hFlush stdout
      value <- getLine
      putStrLn ""
      print (searchInTree value tree)
      putStrLn ""
    "7" -> do
      print (inOrder tree)
      putStrLn ""
    "8" -> do
      print (preOrder tree)
      putStrLn ""
    "9" -> do
      print (postOrder tree)
      putStrLn ""
    "10" -> do
      print (treeHeight tree)
      putStrLn ""
    "11" -> do
      print (isBalanced tree)
      putStrLn ""
    "12" -> exitSuccess
    _ -> putStrLn "Entrada inválida.\n"
  main