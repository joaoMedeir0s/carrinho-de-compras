module Menu.UserMenuController where 
import Util
import Usuario
paginaDoUsuarioMenu :: Usuario -> IO ()
paginaDoUsuarioMenu usuario = do 
    paginaDoUsuario <- readFile "Menu/SpritesMenu/pagina_do_usuario.txt"
    userChoice <- inputComMenu paginaDoUsuario ""
    forkUserChoice userChoice

forkUserChoice :: String -> IO()
forkUserChoice userChoice = do
    putStrLn userChoice
