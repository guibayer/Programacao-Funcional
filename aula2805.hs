--iSort :: [String]->[String]
--iSort [] = []
--iSort (x:xs) = ins x (iSort xs)

--ins :: String -> [String] -> [String]
--ins a [] = [a]
--ins a (x:xs)
-- | a <= x = a : x : xs
-- | otherwise = x : ins a xs

--main :: IO ()
--main = do
-- nomes <- leNomes
-- putStr (foldr (++) [] (map (++"\n") (iSort nomes)))

--leNomes :: IO [String]
--leNomes = do
-- nome <- getLine
-- if(nome == "")
--  then return []
--  else do
--   nomes <- leNomes
--   return (nome : nomes)

data Temperatura = Frio | Calor
 deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
 deriving(Eq,Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio


data Funcionario = Pessoa Nome Idade
 deriving(Eq,Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre = Pessoa "Andre Du Bois" 28

getNome :: Funcionario -> Nome
getNome (Pessoa n i) = n

getIdade :: Funcionario -> Idade
getIdade (Pessoa n i) = i

data Forma = Circulo Float | Retangulo Float Float
 deriving(Eq,Show)

meuCirculo :: Forma
meuCirculo = Circulo 2

meuRetangulo :: Forma
meuRetangulo = Retangulo 2 4

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a
