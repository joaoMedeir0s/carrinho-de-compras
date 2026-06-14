module Util where
import System.Console.Haskeline

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