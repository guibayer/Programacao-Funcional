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

vendaTotal :: Int -> Int
vendaTotal n
 | n == 0      = vendas 0
 | otherwise   = vendas n + vendaTotal (n-1)
