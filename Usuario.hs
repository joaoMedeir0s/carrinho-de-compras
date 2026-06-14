module Usuario
(
    Usuario(..),
    cadastrarUsuario,
    autenticarUsuario,
    usuarioExiste,
    buscarUsuario
) where

data Usuario = Usuario
    {
        nome  :: String,
        email :: String,
        senha :: String
    }
    deriving (Show, Eq)

-- Verifica se já existe um usuário com o email informado
usuarioExiste :: [Usuario] -> String -> Bool
usuarioExiste usuarios emailUsuario =
    any (\u -> email u == emailUsuario) usuarios

-- Busca um usuário pelo email
buscarUsuario :: [Usuario] -> String -> Maybe Usuario
buscarUsuario [] _ = Nothing
buscarUsuario (u:us) emailUsuario
    | email u == emailUsuario = Just u
    | otherwise               = buscarUsuario us emailUsuario

-- Cadastra um novo usuário

-- Retorna Nothing caso o email já exista
cadastrarUsuario :: [Usuario]
                -> String
                -> String
                -> String
                -> Maybe [Usuario]

cadastrarUsuario usuarios nomeUsuario emailUsuario senhaUsuario
    | usuarioExiste usuarios emailUsuario = Nothing
    | otherwise =
        Just (Usuario
                { nome = nomeUsuario
                , email = emailUsuario
                , senha = senhaUsuario
                } : usuarios)

-- Realiza autenticação
-- Retorna o usuário autenticado ou Nothing
autenticarUsuario :: [Usuario]
                -> String
                -> String
                -> Maybe Usuario

autenticarUsuario [] _ _ = Nothing

autenticarUsuario (u:us) emailUsuario senhaDigitada
    | email u == emailUsuario &&
    senha u == senhaDigitada = Just u
    | otherwise = autenticarUsuario us emailUsuario senhaDigitada