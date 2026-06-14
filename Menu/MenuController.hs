module Menu.MenuController where
import Usuario
import System.IO ( hFlush, stdout )
import System.Console.Haskeline
import Control.Concurrent (threadDelay)
import Util
import Menu.UserMenuController (paginaDoUsuarioMenu)
startMenu :: [Usuario] -> IO()
startMenu listaUsuario = do
    limparTela
    startMenuText <- readFile "Menu/SpritesMenu/start_menu.txt"
    userOption <- inputComMenu startMenuText "Mensagem de erro"

    userChoiceStartMenu userOption listaUsuario

{-limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"-}

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
    putStrLn loginFeitoText
    threadDelay 2000000
    paginaDoUsuarioMenu usuario

     

loginFailedMenu :: [Usuario] -> IO ()
loginFailedMenu listaUsuario = do
    loginFalhoMensagem <- readFile "Menu/SpritesMenu/Login/login_falhou_triste_menu.txt"
    putStrLn loginFalhoMensagem
    threadDelay 1500000
    tenteNovamente <- readFile "Menu/SpritesMenu/tente_novamente.txt" 
    putStrLn tenteNovamente
    threadDelay 1000000
    startMenu listaUsuario


inputSenhaUsuario :: String -> String -> IO String
inputSenhaUsuario menu mensagemDeErro = do
    limparTela
    putStrLn menu
    userSenha <- runInputT defaultSettings $ getPassword(Just '*') "Digite sua senha: "
    case userSenha of
        Just p -> return p
        Nothing -> do
            putStrLn mensagemDeErro 
            inputSenhaUsuario menu mensagemDeErro
            return ""


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
    cadastroRealizadoMensagem <- readFile "Menu/SpritesMenu/Cadastro/cadastro_realizado.txt"
    putStrLn cadastroRealizadoMensagem
    threadDelay 1500000
    falaLoginMensagem <- readFile "Menu/SpritesMenu/Login/faca_login.txt"
    putStrLn falaLoginMensagem
    threadDelay 1000000
    startMenu listaUsuario

cadastroFalhou :: [Usuario] -> IO()
cadastroFalhou listaUsuario = do
    usuarioExistente <- readFile "Menu/SpritesMenu/Cadastro/usuario_ja_existe.txt"
    putStrLn usuarioExistente
    threadDelay 2000000
    startMenu listaUsuario

{-inputComMenu :: String -> String -> IO String
inputComMenu menu mensagemDeErro = do
    limparTela
    putStrLn menu
    userData <-  runInputT defaultSettings $ getInputLine "Digite aqui: "
    case userData of
        Just d -> return d
        Nothing -> do
            putStrLn mensagemDeErro
            inputComMenu menu mensagemDeErro -}