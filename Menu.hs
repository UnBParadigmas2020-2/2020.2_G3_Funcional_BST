module Menu (menu) where

menu :: IO ()
menu = do
  putStr
    "Funções disponíveis\n\n1. Imprimir árvore\n2. Inserir elemento\n3. Inserir vários elementos\n4. Deletar elemento\
    \\n5. Deletar vários elementos\n6. Procurar elemento na árvore\n7. Imprimir in order\n8. Imprimir pre order\n9. Imprimir pos order\n\
    \10. Altura da ávore\n11. Checar se a árvore é balanceada\n12. Sair\n\nEntrada: "