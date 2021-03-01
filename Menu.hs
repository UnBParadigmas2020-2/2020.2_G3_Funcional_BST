module Menu (menu) where

menu :: IO ()
menu = do
  putStrLn
    "Funções disponíveis\n1. Imprimir árvore\n2. Inserir elemento\n3. Deletar elemento\
    \\n4. Imprimir in order\n5. Imprimir pre order\n6. Imprimir pos order\n\
    \7. Altura da ávore\n8. Checar se a árvore é balanceada\nEntrada:"