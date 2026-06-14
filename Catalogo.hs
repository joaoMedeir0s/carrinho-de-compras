module Catalogo
(
    listarEmEstoque,
    buscarPorCategoria,
    buscarPorNome,
    atualizarEstoque
) where

import Data.List (isInfixOf)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Map (Map)
import Types

-- Retorna apenas os produtos que possuem estoque maior que zero
listarEmEstoque :: Map Int Produto -> [Produto]
listarEmEstoque produtos = filter (\p -> prodEstoque p > 0) (Map.elems produtos)

-- Busca produtos por uma categoria específica
buscarPorCategoria :: Map Int Produto -> String -> [Produto]
buscarPorCategoria produtos cat = filter (\p -> prodCategoria p == cat) (Map.elems produtos)

-- Busca produtos pelo nome (ignora maiúsculas e minúsculas)
buscarPorNome :: Map Int Produto -> String -> [Produto]
buscarPorNome produtos termo = 
    filter (\p -> map toLower termo `isInfixOf` map toLower (prodNome p)) (Map.elems produtos)

-- Atualiza o estoque de um produto específico
atualizarEstoque :: Map Int Produto -> Int -> Int -> Map Int Produto
atualizarEstoque produtos idBuscado novoEstoque =
    Map.adjust (\p -> p { prodEstoque = novoEstoque }) idBuscado produtos
