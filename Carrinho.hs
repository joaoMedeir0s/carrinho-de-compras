module Carrinho( visualizarCarrinho, descreverCarrinho, adicionarAoCarrinho, removerDoCarrinho, atualizarQuantidade) where

import qualified Data.Map as M
import Types

visualizarCarrinho :: Catalogo -> Carrinho -> [(Produto, Quantidade)]
visualizarCarrinho catalogo carrinho =
    [ (produto, qtd)
    | (pid, qtd) <- M.toList carrinho
    , Just produto <- [M.lookup pid catalogo]
    ]

descreverCarrinho :: Catalogo -> Carrinho -> [String]
descreverCarrinho catalogo carrinho =
    map descreverItem (visualizarCarrinho catalogo carrinho)
  where
    descreverItem (produto, qtd) =
        prodNome produto
        ++ "  (ID " ++ show (prodId produto) ++ ")"
        ++ "  | Qtd: " ++ show qtd
        ++ "  | Unit.: R$ " ++ show (prodPreco produto)
        ++ "  | Subtotal: R$ " ++ show (prodPreco produto * fromIntegral qtd)

adicionarAoCarrinho :: ProdutoID -> Quantidade -> Catalogo -> Carrinho -> Either String Carrinho
adicionarAoCarrinho pid qtd catalogo carrinho
    | qtd <= 0  = Left "A quantidade a adicionar deve ser maior que zero."
    | otherwise =
        case M.lookup pid catalogo of
            Nothing      -> Left ("Produto com ID " ++ show pid ++ " não existe no catálogo.")
            Just produto ->
                let qtdAtual = M.findWithDefault 0 pid carrinho
                    qtdTotal = qtdAtual + qtd
                in if qtdTotal > prodEstoque produto
                      then Left ("Estoque insuficiente para '" ++ prodNome produto
                                 ++ "'. Disponível: " ++ show (prodEstoque produto)
                                 ++ ", total solicitado: " ++ show qtdTotal ++ ".")
                      else Right (M.insert pid qtdTotal carrinho)

removerDoCarrinho :: ProdutoID -> Carrinho -> Carrinho
removerDoCarrinho pid carrinho = M.delete pid carrinho

atualizarQuantidade :: ProdutoID -> Quantidade -> Catalogo -> Carrinho -> Either String Carrinho
atualizarQuantidade pid novaQtd catalogo carrinho
    | novaQtd <= 0 = Right (M.delete pid carrinho)
    | otherwise =
        case M.lookup pid catalogo of
            Nothing      -> Left ("Produto com ID " ++ show pid ++ " não existe no catálogo.")
            Just produto ->
                if novaQtd > prodEstoque produto
                   then Left ("Estoque insuficiente para '" ++ prodNome produto
                              ++ "'. Disponível: " ++ show (prodEstoque produto)
                              ++ ", solicitado: " ++ show novaQtd ++ ".")
                   else Right (M.insert pid novaQtd carrinho)