module Produto
(
    Produto(..),
    listarEmEstoque,
    buscarPorCategoria,
    buscarPorNome,
    atualizarEstoque
) where

import Data.List (isInfixOf)
import Data.Char (toLower)

data Produto = Produto
    { idProduto :: Int
    , nome      :: String
    , preco     :: Double
    , descricao :: String
    , estoque   :: Int
    , categoria :: String
    } deriving (Show, Eq)

-- Retorna apenas os produtos que possuem estoque maior que zero
listarEmEstoque :: [Produto] -> [Produto]
listarEmEstoque produtos = filter (\p -> estoque p > 0) produtos

-- Busca produtos por uma categoria específica
buscarPorCategoria :: [Produto] -> String -> [Produto]
buscarPorCategoria produtos cat = filter (\p -> categoria p == cat) produtos

-- Busca produtos pelo nome (ignora maiúsculas e minúsculas)
buscarPorNome :: [Produto] -> String -> [Produto]
buscarPorNome produtos termo = 
    filter (\p -> map toLower termo `isInfixOf` map toLower (nome p)) produtos

-- Atualiza o estoque de um produto específico na lista buscando pelo ID
atualizarEstoque :: [Produto] -> Int -> Int -> [Produto]
atualizarEstoque [] _ _ = []
atualizarEstoque (p:ps) idBuscado novoEstoque
    | idProduto p == idBuscado = p { estoque = novoEstoque } : ps
    | otherwise                = p : atualizarEstoque ps idBuscado novoEstoque
