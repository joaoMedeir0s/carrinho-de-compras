module Menu.MenuController where
import Usuario
import System.IO ( hFlush, stdout )
import System.Console.Haskeline
import Control.Concurrent (threadDelay)
import Util
import Menu.UserMenuController (paginaDoUsuarioMenu)
import Types
import qualified Data.Map as M

startMenu :: [Usuario] -> IO()
startMenu listaUsuario = do
    limparTela
    startMenuText <- readFile "Menu/SpritesMenu/start_menu.txt"
    userOption <- inputComMenu startMenuText "Mensagem de erro"

    userChoiceStartMenu userOption listaUsuario

userChoiceStartMenu :: String -> [Usuario] -> IO()
userChoiceStartMenu userChoice listaUsuario 
    | userChoice == "L" || userChoice == "l" = menuLoginUsuario listaUsuario
    | userChoice == "C" || userChoice == "c" = menuCadastroUsuario listaUsuario
    | otherwise = do
        putStrLn "Opção Inválida!"
        startMenu listaUsuario 

menuLoginUsuario :: [Usuario] -> IO()
menuLoginUsuario listaUsuario = do
    menuEmail <- readFile "Menu/SpritesMenu/Login/login_menu_email.txt"
    menuSenha <- readFile "Menu/SpritesMenu/Login/login_menu_senha.txt"
    email <- inputComMenu menuEmail "Email Inválido"
    senha <- inputSenhaUsuario  menuSenha "Senha inválida"
    let tentativa = autenticarUsuario listaUsuario email senha
    case tentativa of
        Just t -> loginFeitoMenu t
        Nothing -> loginFailedMenu listaUsuario

loginFeitoMenu :: Usuario -> IO()
loginFeitoMenu usuario = do
    loginFeitoText <- readFile "Menu/SpritesMenu/Login/login_feito_feliz_menu.txt"
    limparTela
    putStrLn loginFeitoText
    threadDelay 2000000
    catalogo <- carregarCatalogo
    paginaDoUsuarioMenu usuario catalogo

     

loginFailedMenu :: [Usuario] -> IO ()
loginFailedMenu listaUsuario = do
    limparTela
    loginFalhoMensagem <- readFile "Menu/SpritesMenu/Login/login_falhou_triste_menu.txt"
    putStrLn loginFalhoMensagem
    threadDelay 1500000
    limparTela
    tenteNovamente <- readFile "Menu/SpritesMenu/tente_novamente.txt" 
    putStrLn tenteNovamente
    threadDelay 1000000
    startMenu listaUsuario

menuCadastroUsuario :: [Usuario] -> IO()
menuCadastroUsuario listaUsuario = do
    cadastroNomeMenu <- readFile "Menu/SpritesMenu/Cadastro/cadastro_nome_menu.txt"
    cadastroEmailMenu <- readFile "Menu/SpritesMenu/Cadastro/cadastro_email_menu.txt"
    cadastroSenhaMenu <- readFile "Menu/SpritesMenu/Cadastro/cadastro_senha_menu.txt"
    nome <- inputComMenu cadastroNomeMenu "Digite um nome"
    email <- inputComMenu cadastroEmailMenu "Digite um email"
    senha <- inputSenhaUsuario cadastroSenhaMenu "Digite uma senha: "

    let novaLista = cadastrarUsuario listaUsuario nome email senha
    case novaLista of
        Just listaAtualizada -> cadastroRealizado listaAtualizada
        Nothing  -> cadastroFalhou listaUsuario 

cadastroRealizado :: [Usuario] -> IO()
cadastroRealizado listaUsuario = do
    limparTela
    cadastroRealizadoMensagem <- readFile "Menu/SpritesMenu/Cadastro/cadastro_realizado.txt"
    putStrLn cadastroRealizadoMensagem
    threadDelay 1500000
    limparTela
    falaLoginMensagem <- readFile "Menu/SpritesMenu/Login/faca_login.txt"
    putStrLn falaLoginMensagem
    threadDelay 1000000
    startMenu listaUsuario

cadastroFalhou :: [Usuario] -> IO()
cadastroFalhou listaUsuario = do
    limparTela
    usuarioExistente <- readFile "Menu/SpritesMenu/Cadastro/usuario_ja_existe.txt"
    putStrLn usuarioExistente
    threadDelay 2000000
    startMenu listaUsuario

carregarCatalogo :: IO Catalogo
carregarCatalogo = do
  conteudo <- readFile "catalogo.txt"
  let linhas   = lines conteudo
  let produtos = map parseProduto linhas
  return (M.fromList [(prodId p, p) | p <- produtos])


parseProduto :: String -> Produto
parseProduto linha =
  let [idStr, nome, precoStr, desc, estoqueStr, categoria] = splitOn ',' linha
  in Produto
      { prodId        = read idStr
      , prodNome      = nome
      , prodPreco     = read precoStr
      , prodDesc      = desc
      , prodEstoque   = read estoqueStr
      , prodCategoria = categoria
      }


splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim (c:cs)
  | c == delim = "" : resto
  | otherwise  = (c : head resto) : tail resto
  where resto = splitOn delim cs