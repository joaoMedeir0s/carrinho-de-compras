module Menu.MenuController where

import System.IO ( hFlush, stdout )
import System.Console.Haskeline

startMenu :: IO()
startMenu = do
    limparTela
    startMenuText <- readFile "Menu/SpritesMenu/start_menu.txt"
    userOption <- inputComMenu startMenuText "Mensagem de erro"

    userChoiceStartMenu userOption

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

userChoiceStartMenu :: String -> IO()
userChoiceStartMenu userChoice
    | userChoice == "L" || userChoice == "l" = menuLoginUsuario
    | userChoice == "C" || userChoice == "c" = menuCadastroUsuario
    | otherwise = do
        putStrLn "Opção Inválida!"
        startMenu

menuLoginUsuario :: IO()
menuLoginUsuario = do
    menuEmail <- readFile "Menu/SpritesMenu/login_menu_email.txt"
    email <- inputComMenu menuEmail "Email Inválido"
    senha <- inputSenhaUsuario

    putStrLn email
    putStrLn senha


inputSenhaUsuario :: IO String
inputSenhaUsuario = do
    limparTela
    menuLoginSenhaText <- readFile "Menu/SpritesMenu/login_menu_senha.txt"
    putStrLn menuLoginSenhaText
    userSenha <- runInputT defaultSettings $ getPassword(Just '*') "Digite aqui: "
    case userSenha of
        Just p -> return p
        Nothing -> do
            putStrLn "Senha vazia! Tente novamente"
            inputSenhaUsuario
            return ""


--- TODO 
verificarLogin :: String -> String -> Bool
verificarLogin email senha = senha == email
        
menuCadastroUsuario :: IO()
menuCadastroUsuario = do
    cadastroNomeMenu <- readFile "Menu/SpritesMenu/cadastro_nome_menu.txt"
    cadastroEmailMenu <- readFile "Menu/SpritesMenu/cadastro_email_menu.txt"
    cadastroSenhaMenu <- readFile "Menu/SpritesMenu/cadastro_senha_menu.txt"
    nome <- inputComMenu cadastroNomeMenu "Digite um nome"
    email <- inputComMenu cadastroEmailMenu "Digite um email"
    senha <- inputSenhaUsuario

    putStrLn nome
    putStrLn email 
    putStrLn senha 


inputComMenu :: String -> String -> IO String
inputComMenu menu mensagemDeErro = do
    limparTela
    putStrLn menu
    userData <-  runInputT defaultSettings $ getInputLine "Digite aqui: "
    case userData of
        Just d -> return d
        Nothing -> do
            putStrLn mensagemDeErro
            inputComMenu menu mensagemDeErro