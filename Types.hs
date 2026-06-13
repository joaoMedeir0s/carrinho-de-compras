module Types where

import qualified Data.Map as M

type ProdutoID  = Int
type Quantidade = Int

data Produto = Produto
    { prodId      :: ProdutoID
    , prodNome    :: String
    , prodPreco   :: Double
    , prodDesc    :: String
    , prodEstoque :: Quantidade
    } deriving (Show, Eq)

type Catalogo = M.Map ProdutoID Produto
type Carrinho = M.Map ProdutoID Quantidade