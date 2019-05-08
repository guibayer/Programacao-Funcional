-- Funções de alta ordem

times2,times3 :: Int -> Int

times2 n = 2*n
times3 n = 3*n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (a:x) = f a : mapInt f x

-- 1. Defina novamente a fun¸c˜ao total, que calcula o total de vendas de v´arias semanas. Agora a fun¸c˜ao total deve 
--receber como argumento a fun¸c˜ao que determina as vendas de uma semana:

--total :: (Int -> Int) -> Int -> Int

-- 2.

foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "lista vazia nao e permitido"
foldInt f [x] = x
foldInt f (x:xs) = f x (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y


-- 3. testar função foldInt com mais dois exemplos

mult :: Int -> Int -> Int
mult x y = x * y

subtracao :: Int -> Int -> Int
subtracao x y = x - y 

-- 4. filterString :: (Char -> Bool) -> [Char] -> [Char]

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f [x] = [x]
filterString f (x:xs) 
 | f x = x : filterString f xs
 | otherwise = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

-- 5. Testar filterString com mais dois exemplos

naoLetraA :: Char -> Bool
naoLetraA x = x /= 'a'

naoLetraG :: Char -> Bool
naoLetraG x = x /= 'G'

-- 6. Usando as fun¸c˜oes de alta ordem definidas nos exerc´ıcios anteriores (incluindo o mapInt), defina uma fun¸c˜ao que devolva a soma do quadrado dos
--n´umeros em uma lista l