module Util where
import System.Console.Haskeline
import Control.Concurrent

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

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

exibirOpcaoInvalida :: IO()
exibirOpcaoInvalida = do
    mensagemDeErro <- readFile "Menu/SpritesMenu/opcao_invalida.txt"
    putStrLn mensagemDeErro

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

esperarEnter :: IO ()
esperarEnter = do
  putStrLn "\nPressione Enter para continuar..."
  _ <- getLine
  return ()

produtoInvalido :: IO()
produtoInvalido = do
    msg <- readFile "Menu/SpritesMenu/Carrinho/produto_invalido.txt"
    limparTela
    putStrLn msg
    threadDelay 2000000

produtoAdicionado :: IO()
produtoAdicionado = do
    msg <- readFile "Menu/SpritesMenu/Carrinho/produto_adicionado.txt"
    limparTela
    putStrLn msg
    threadDelay 2000000
