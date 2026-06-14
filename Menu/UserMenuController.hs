module Menu.UserMenuController where 
import Util
import Usuario
import Control.Concurrent

paginaDoUsuarioMenu :: Usuario -> IO ()
paginaDoUsuarioMenu usuario = do 
    paginaDoUsuario <- readFile "Menu/SpritesMenu/pagina_do_usuario.txt"
    userChoice <- inputComMenu paginaDoUsuario ""
    forkUserChoice usuario userChoice

forkUserChoice :: Usuario -> String -> IO()
forkUserChoice usuario userChoice 
    | userChoice == "v" || userChoice == "V" = verCatalogo
    | userChoice == "a" || userChoice == "A" = acessarCarrinho
    | userChoice == "f" || userChoice == "F" = finalizarCompra
    | userChoice == "s" || userChoice == "S" = finalizarSessao
    | otherwise = do 
        exibirOpcaoInvalida
        threadDelay 1000000
        paginaDoUsuarioMenu usuario

verCatalogo :: IO()
verCatalogo = do
    putStrLn "a"

acessarCarrinho :: IO()
acessarCarrinho = do
    putStrLn "TODO"


finalizarCompra :: IO()
finalizarCompra = do
    putStrLn "TODO"


finalizarSessao :: IO()
finalizarSessao = do
    putStrLn "TODO"