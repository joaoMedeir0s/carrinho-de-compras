module Finalizacao where

import qualified Data.Map as M
import Types
import Carrinho
import Usuario
import qualified Catalogo

data ResumoPedido = ResumoPedido
    { cliente  :: Usuario
    , itens    :: [(Produto, Quantidade)]
    , total    :: Double
    , mensagem :: String
    } deriving (Show, Eq)

formatarItens :: [(Produto, Quantidade)] -> String
formatarItens [] = ""
formatarItens ((prod, qtd):restantes) =
    "- " ++ prodNome prod ++ " (x" ++ show qtd ++ "): R$ " ++ show (prodPreco prod * fromIntegral qtd) ++ "\n"
    ++ formatarItens restantes

gerarMensagemSucesso :: Usuario -> [(Produto, Quantidade)] -> Double -> String
gerarMensagemSucesso user listagem totalVal =
    "===========================================\n" ++
    "             COMPRA CONCLUIDA              \n" ++
    "===========================================\n" ++
    "Cliente: " ++ nome user ++ " (" ++ email user ++ ")\n" ++
    "-------------------------------------------\n" ++
    "Itens:\n" ++
    formatarItens listagem ++
    "-------------------------------------------\n" ++
    "Total Pago: R$ " ++ show totalVal ++ "\n" ++
    "==========================================="

atualizarEstoqueDoPedido :: Catalogo -> [(ProdutoID, Quantidade)] -> Either String Catalogo
atualizarEstoqueDoPedido cat [] = Right cat
atualizarEstoqueDoPedido cat ((pid, qtd):restantes) =
    case M.lookup pid cat of
        Nothing -> Left ("Produto com ID " ++ show pid ++ " nao existe no catalogo.")
        Just prod ->
            if prodEstoque prod < qtd
                then Left ("Estoque insuficiente para '" ++ prodNome prod ++ "'.")
                else
                    let novoEstoque = prodEstoque prod - qtd
                        novoCat = Catalogo.atualizarEstoque cat pid novoEstoque
                    in atualizarEstoqueDoPedido novoCat restantes

calcularTotal :: Catalogo -> [(ProdutoID, Quantidade)] -> Double
calcularTotal _ [] = 0.0
calcularTotal cat ((pid, qtd):restantes) =
    case M.lookup pid cat of
        Nothing -> calcularTotal cat restantes
        Just prod -> (prodPreco prod * fromIntegral qtd) + calcularTotal cat restantes

finalizarCompra :: Usuario -> Catalogo -> Carrinho -> Either String (Catalogo, ResumoPedido)
finalizarCompra user cat carrinho =
    if M.null carrinho
        then Left "Erro: O carrinho esta vazio!"
        else
            case atualizarEstoqueDoPedido cat (M.toList carrinho) of
                Left erro -> Left erro
                Right novoCat ->
                    let listagemItens = visualizarCarrinho cat carrinho
                        totalVal = calcularTotal cat (M.toList carrinho)
                        msg = gerarMensagemSucesso user listagemItens totalVal
                        resumo = ResumoPedido
                            { cliente  = user
                            , itens    = listagemItens
                            , total    = totalVal
                            , mensagem = msg
                            }
                    in Right (novoCat, resumo)
