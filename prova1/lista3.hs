type TresInteiros = (Int, Int, Int)

adicionaTupla :: TresInteiros -> Int
adicionaTupla (x,y,z) = x + y + z

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift (n, t) = (t, n)

maximo :: Int -> Int -> Int
maximo x y
 | x > y = x
 | otherwise = y

minimo :: Int -> Int -> Int
minimo x y 
 | x < y = x
 | otherwise = y

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax x y z = (minimo (minimo x y) z, maximo (maximo x y) z)

ordenaTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTupla (x,y,z) = (minimo (minimo x y) z, maximo (minimo x y) z , maximo (maximo x y) z)

vendas :: Int -> Int
vendas 0 = 2
vendas 1 = 0
vendas 2 = 4
vendas 3 = 3
vendas _ = 0

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda x = maximo (vendas x) (maiorVenda (x-1))

maxVenda :: Int -> Int
maxVenda x 
 | maiorVenda x == vendas x = x
 | otherwise = maxVenda (x-1)

zeroVendas :: Int -> (Int, Bool)
zeroVendas x
 | vendas x == 0 = (x, True)
 | otherwise = (-1, False)

type Livro = (String, String, Int)

nomeDoLivro :: Livro -> String
nomeDoLivro (x, y, z) = x

autorDoLivro :: Livro -> String
autorDoLivro (x, y, z) = y

isbnDoLivro :: Livro -> Int
isbnDoLivro (x, y, z) = z