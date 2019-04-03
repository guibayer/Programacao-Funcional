somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista x

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (a:x) = (2 * a) : dobraLista x

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (a:x) = a * produtoLista x 

andLista :: [Bool] -> Bool
andLista [] = True
andLista (a:x) = a && andLista x

concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (a:x) = a ++ concatLista x

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (b:x)
 | a <= b = a:(b:x)
 | otherwise = b: ins a x 

iSort :: [Int] -> [Int]
iSort [] = []
iSort (a:x) = ins a (iSort x)

isIn :: Int -> [Int] -> Bool
isIn a [] = [a]
isIn a (b:x)
 | a == b = True
 | otherwise = False

membro :: [Int] -> Int -> Bool
membro [] v = [] v
membro (a:x) v = isIn a v || membro v x
