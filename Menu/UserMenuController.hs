module Menu.UserMenuController where 
import Util
import Usuario
import Control.Concurrent
import Carrinho (descreverCarrinho, adicionarAoCarrinho, removerDoCarrinho)
import Catalogo
import Types
import Data.List(intercalate)
import Finalizacao

paginaDoUsuarioMenu :: Usuario -> Catalogo -> IO ()
paginaDoUsuarioMenu usuario catalogo = do 
    paginaDoUsuario <- readFile "Menu/SpritesMenu/pagina_do_usuario.txt"
    userChoice <- inputComMenu paginaDoUsuario ""
    forkUserChoice usuario catalogo userChoice 

forkUserChoice :: Usuario -> Catalogo -> String -> IO()
forkUserChoice usuario catalogo userChoice 
    | userChoice == "v" || userChoice == "V" = verCatalogo usuario catalogo --- feito
    | userChoice == "a" || userChoice == "A" = acessarCarrinho catalogo usuario -- feito
    | userChoice == "f" || userChoice == "F" = finalizacaoCompra usuario catalogo (carrinho usuario)
    | userChoice == "s" || userChoice == "S" = finalizarSessao -- feito
    | otherwise = do 
        exibirOpcaoInvalida
        threadDelay 1000000
        paginaDoUsuarioMenu usuario catalogo


verCatalogo :: Usuario -> Catalogo -> IO ()
verCatalogo usuario catalogo = do
  limparTela
  putStrLn (estoqueParaString catalogo)
  esperarEnter
  paginaDoUsuarioMenu usuario catalogo

estoqueParaString :: Catalogo -> String
estoqueParaString catalogo =
  let produtos = listarEmEstoque catalogo
      linhas   = map formatarProduto produtos
  in intercalate "\n" linhas

formatarProduto :: Produto -> String
formatarProduto p =
  "[" ++ show (prodId p) ++ "] "
  ++ prodNome p
  ++ " | R$ " ++ show (prodPreco p)
  ++ " | Estoque: " ++ show (prodEstoque p)
  ++ " | " ++ prodCategoria p

acessarCarrinho :: Catalogo -> Usuario -> IO()
acessarCarrinho catalogo usuario = do
    carrinhoMenu <- readFile "Menu/SpritesMenu/Carrinho/menu_carrinho.txt"
    userChoice <- inputComMenu carrinhoMenu "" 
    carrinhoUserChoice userChoice catalogo usuario

carrinhoUserChoice :: String -> Catalogo -> Usuario -> IO()
carrinhoUserChoice userChoice catalogo usuario
    | userChoice == "v" || userChoice == "V" = verCarrinho catalogo usuario -- feito
    | userChoice == "a" || userChoice == "A" = adicionarAoMeuCarrinho catalogo usuario --feito 
    | userChoice == "r" || userChoice == "R" = removerItemDoCarrinho catalogo usuario --feito (menos a parte de lidar com id inválido)
    | userChoice == "s" || userChoice == "S" = paginaDoUsuarioMenu usuario catalogo --feito 
    |otherwise = do
        exibirOpcaoInvalida
        acessarCarrinho catalogo usuario

verCarrinho :: Catalogo -> Usuario -> IO()
verCarrinho catalogo usuario = do
    let linhas = descreverCarrinho catalogo (carrinho usuario)
    limparTela
    putStrLn (unlines linhas)
    esperarEnter
    acessarCarrinho catalogo usuario
    

adicionarAoMeuCarrinho :: Catalogo -> Usuario -> IO()
adicionarAoMeuCarrinho catalogo usuario = do
    askIdMenu <- readFile "Menu/SpritesMenu/Carrinho/adicionar_id.txt"
    prodId <- inputComMenu askIdMenu ""
    askQantMenu <- readFile "Menu/SpritesMenu/Carrinho/adicionar_qant.txt"
    qant <- inputComMenu askQantMenu ""
    case adicionarAoCarrinho (read prodId) (read qant) catalogo (carrinho usuario) of
        Left erro -> do
            produtoInvalido
            paginaDoUsuarioMenu usuario catalogo
        Right novoCarrinho -> do
            let usuarioAtualizado = usuario { carrinho = novoCarrinho }
            produtoAdicionado
            paginaDoUsuarioMenu usuarioAtualizado catalogo
            

removerItemDoCarrinho :: Catalogo -> Usuario -> IO()
removerItemDoCarrinho catalogo usuario = do
    menuRemocao <- readFile "Menu/SpritesMenu/Carrinho/remover_id.txt"
    prodId <- inputComMenu menuRemocao ""
    let novoCarrinho      = removerDoCarrinho (read prodId) (carrinho usuario)
    let usuarioAtualizado = usuario { carrinho = novoCarrinho }
    remocaoRealizada <- readFile "Menu/SpritesMenu/Carrinho/remocao_realizada.txt"
    limparTela
    putStrLn remocaoRealizada
    threadDelay 1500000
    paginaDoUsuarioMenu usuarioAtualizado catalogo
 
finalizacaoCompra :: Usuario -> Catalogo -> Carrinho ->  IO()
finalizacaoCompra usuario catalogo carrinho = do
    case finalizarCompra usuario catalogo carrinho of
        Left erro -> putStrLn erro
        Right (novoCat, resumo) -> do
            limparTela
            putStrLn (mensagem resumo)
            esperarEnter
            paginaDoUsuarioMenu usuario novoCat



finalizarSessao :: IO()
finalizarSessao = do
    limparTela
    adeus <- readFile "Menu/SpritesMenu/adeus.txt"
    putStrLn adeus 