module Carrinho( visualizarCarrinho, descreverCarrinho, adicionarAoCarrinho, removerDoCarrinho, atualizarQuantidade) where

import qualified Data.Map as M
import Types

-- Lista os itens do carrinho junto com os dados do produto e a quantidade
visualizarCarrinho :: Catalogo -> Carrinho -> [(Produto, Quantidade)]
visualizarCarrinho catalogo carrinho =
    [ (produto, qtd)
    | (pid, qtd) <- M.toList carrinho
    , Just produto <- [M.lookup pid catalogo]
    ]

-- Monta uma linha de texto legível para cada item do carrinho
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

-- Adiciona um produto ao carrinho, barrando se ultrapassar o estoque
adicionarAoCarrinho :: ProdutoID -> Quantidade -> Catalogo -> Carrinho -> Either String Carrinho
adicionarAoCarrinho pid qtd catalogo carrinho
    | qtd <= 0  = Left "A quantidade a adicionar deve ser maior que zero."
    | otherwise =
        case M.lookup pid catalogo of
            Nothing      -> Left ("Produto com ID " ++ show pid ++ " não existe no catálogo.")
            Just produto ->
                let qtdAtual = M.findWithDefault 0 pid carrinho   -- quanto já existe no carrinho
                    qtdTotal = qtdAtual + qtd                     -- total após a adição
                in if qtdTotal > prodEstoque produto
                      then Left ("Estoque insuficiente para '" ++ prodNome produto
                                 ++ "'. Disponível: " ++ show (prodEstoque produto)
                                 ++ ", total solicitado: " ++ show qtdTotal ++ ".")
                      else Right (M.insert pid qtdTotal carrinho)

-- Remove por completo um produto do carrinho pelo ID
removerDoCarrinho :: ProdutoID -> Carrinho -> Carrinho
removerDoCarrinho pid carrinho = M.delete pid carrinho

-- Altera a quantidade de um item; se for <= 0 remove, senão valida o estoque
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