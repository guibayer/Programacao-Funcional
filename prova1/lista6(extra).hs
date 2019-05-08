retornaUltimo :: [Int] -> Int
retornaUltimo [] = 0
retornaUltimo (x:xs)
 | xs == [] = x
 | otherwise = retornaUltimo xs

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao n [] = 0
pegaPosicao n (x:xs)
 | n == 1 = x
 | otherwise = pegaPosicao (n-1) xs

pega :: Int -> [Int] -> [Int]
pega 0 x = []
pega n (x:xs) = x : pega (n-1) xs

retira :: Int -> [Int] -> [Int]
retira 0 x = x
retira n (x:xs) = retira (n-1) xs

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

qtdeElementos :: [Int] -> Int
qtdeElementos [] = 0
qtdeElementos (x:xs) = 1 + qtdeElementos xs

media :: [Int] -> Float
media x = (fromIntegral (soma x)) / (fromIntegral(qtdeElementos x))

isMaior :: [Int] -> Int -> Bool
isMaior [] n = True
isMaior (x:xs) n
 | x > n = False
 | otherwise = isMaior xs n

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores 0 x = x
pegaMaiores n (x:xs)
 | isMaior xs x = x : pegaMaiores (n-1) xs 
 | otherwise = pegaMaiores (n-1) xs 

-- 8.
--contaMaiores :: Int -> [Int] -> [Int]

--9. concatena

concatena :: String -> String -> String
concatena x y = x ++ y

concatenaInt :: [Int] -> [Int] -> [Int]
concatenaInt x y = x ++ y 

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = x : intercala xs []
intercala [] (y:ys) = y : intercala [] ys
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 10. compress

proximoIgual :: Char -> [Char] -> Bool
proximoIgual c [] = False
proximoIgual c (x:xs)
 | c == x = True
 | otherwise = False

proximoDiferente :: Char -> [Char] -> Bool
proximoDiferente c [] = True
proximoDiferente c (x:xs)
 | c == x = False
 | otherwise = True

compress :: [Char] -> [Char]
compress [] = []
compress (x:xs)
 | proximoIgual x xs = compress xs
 | otherwise = x : compress xs

-- 11. pack

concatenaIguais :: [Char] -> [Char]
concatenaIguais [] = []
concatenaIguais [x] = []
concatenaIguais (x:xs:xss)
 | x == xs = [x] ++ concatenaIguais xss
 | otherwise = [x]

retiraIguais :: [Char] -> [Char]
retiraIguais [] = []
retiraIguais [x] = []
retiraIguais (x:xs:xss)
 | x == xs = retiraIguais (xss)
 | otherwise = xss

pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs:xss) 
 | x == xs = concatenaIguais xss : pack(retiraIguais xss)
 | otherwise = [] : pack xss

-- Bugado, falta arrumar

-- 12. encode

contaQtdeElementosRepetidos :: [Char] -> Int
contaQtdeElementosRepetidos [] = 1
contaQtdeElementosRepetidos (x:xs)
 | proximoIgual x xs = 1 + contaQtdeElementosRepetidos xs
 | otherwise = 1

encode :: [Char] -> [(Char, Int)]
encode [] = []
encode (x:xs) = (x, 1+contaQtdeElementosRepetidos xs) : encode (retiraIguais(xs))

-- 13. dupli

dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- 14. repli

replicaNumVezes :: Int -> Char -> [Char]
replicaNumVezes 0 v = []
replicaNumVezes n v = v : replicaNumVezes (n-1) v

repli :: Int -> [Char] -> [Char]
repli n [] = []
repli n (x:xs) = replicaNumVezes n x ++ repli n xs

-- 15. dropEvery retira cada nth elemento de uma lista

--dropEvery :: Int -> [Char] -> [Char]
--dropEvery n [] = []
--dropEvery n (x:xs) = 

-- 16. Split

selecionaEsqDaLista :: Int -> [Char] -> [Char]
selecionaEsqDaLista n [] = [] 
selecionaEsqDaLista n (x:xs) 
 | n > 0 = x : selecionaEsqDaLista (n-1) xs
 | otherwise = []

selecionaDirDaLista:: Int -> [Char] -> [Char]
selecionaDirDaLista n [] = []
selecionaDirDaLista n (y:ys) 
 | n /= 0 = selecionaDirDaLista (n-1) ys
 | otherwise = y : selecionaDirDaLista 0 ys

split :: Int -> [Char] -> ([Char],[Char])
split n l = ((selecionaEsqDaLista n l), (selecionaDirDaLista n l))

-- 17. slice

slice :: Int -> Int -> [Char] -> [Char]
slice a b x = sliceAux a b 0 x 

sliceAux :: Int -> Int -> Int -> [Char] -> [Char]
sliceAux a b n [] = []
sliceAux a b n (x:xs)
 | n < a = sliceAux a b (n+1) xs
 | (n+1) > a && n < (b+1) = x : sliceAux a b (n+1) xs
 | otherwise = []

-- 19. removeAt

pegaAtPos :: Int -> [Char] -> Char
pegaAtPos n [] = 'k'
pegaAtPos n (x:xs)
 | n == 1 = x
 | otherwise = pegaAtPos (n-1) xs

removePos :: Int -> [Char] -> [Char]
removePos n [] = []
removePos n (x:xs)
 | n == 1 = xs
 | otherwise = x : removePos (n-1) xs

removeAt :: Int -> [Char] -> (Char, [Char])
removeAt n x = (pegaAtPos n x, removePos n x)
