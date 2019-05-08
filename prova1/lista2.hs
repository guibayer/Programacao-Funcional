-- 1 - Defina uma fun¸c˜ao max :: Int -> Int -> Int que retorna o maior entre dois n´umeros.

maximo :: Int -> Int -> Int
maximo x y
 | x > y = x
 | otherwise = y

-- 2. Usando a fun¸c ao max, defina uma fun¸c˜ao maiorVenda que recebe um argumento num´erico n, e calcule a maior venda em uma 
-- semana entre 0 e n.

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

zeroVendas :: Int -> Int
zeroVendas 0 = vendas 0
zeroVendas x
 | vendas x == 0 = x
 | otherwise = -1

 -- 5. Usando a defini¸c˜ao anterior como guia, defina uma fun¸c˜ao que receba um valor s e uma semana n, e 
 -- devolva qual das semanas entre 0 e n teve vendas iguais a s

valorDeVendaIgual :: Int -> Int -> Int
valorDeVendaIgual s n
 | vendas n == s = n
 | n > 0 = valorDeVendaIgual s (n-1)
 | otherwise = -1

-- 6. Como vocˆe usaria a fun¸c˜ao anterior para definir a fun¸c˜ao zeroVendas

zeroVendas2 :: Int -> Int
zeroVendas2 x = valorDeVendaIgual 0 x 

-- 8. Fatorial em haskell

fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1) 

-- 9. Produto de m e n => m * (m-1) ... n * (n-1)

produtoNeM :: Int -> Int -> Int
produtoNeM n m = fatorial n * fatorial m

-- 10. Fibonacci com os dois primeiros valores sendo 0 e 1

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib(x-2)