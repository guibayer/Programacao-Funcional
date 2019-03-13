idade :: Int  -- Um valor inteiro constante
idade = 17

maiorDeIdade :: Bool       -- Usa a definicao de
maiorDeIdade = (idade>=18) -- idade

quadrado :: Int -> Int   -- funcao que eleva num.
quadrado x = x * x       -- ao quadrado
mini :: Int -> Int -> Int --funcao que mostra
mini a b                  --o menor entre
 | a <= b    = a       -- dois valores
 | otherwise = b

igual :: Int -> Int -> Bool
igual x y =     x == y

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

vendas ::  Int -> Int
vendas 0 = 5
vendas 1 = 5

vendaTotal :: Int -> Int
vendaTotal n
 | n == 0      = vendas 0
 | otherwise   = vendas n + vendaTotal (n-1)

palindrome :: String -> Bool
palindrome s = s == reverse s

n :: Int
n = 0

--Defina a funcaoquantosSaoIguais :: Int -> Int -> Int -> Int que conta quantos argumentos iguais a funcao recebeu
qtdeIguais :: Int -> Int -> Int -> Int
qtdeIguais x y z
 | ( x == y) && ( y == z) && ( x == z) = 3
 | ( x == y) || ( y == z) || ( x == z) = 2
 | otherwise = 0

 -- Defina a funcao
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = ( x /= y) && ( y /= z) && ( x /= z)
