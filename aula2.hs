vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 15
vendas 2 = 30
vendas 4 = 40
vendas _ = 15


vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)

cabecalho :: String
cabecalho = "Semana       Vendas\n"

geraString n =  "\nSemana " ++ show n ++ "      "++ show (vendas n)

geraVendas :: Int -> String
geraVendas 0  = geraString 0
geraVendas n  = geraVendas (n-1) ++ geraString n

tabela :: Int -> String
tabela n = cabecalho ++ geraVendas n ++ "\nTotal :" ++ show (vendaTotal n)

--Exercício 1 -- Lista de Exercícios 2
maximo :: Int -> Int -> Int
maximo x y
 | x > y = x
 | otherwise = y 

--Exercício 2 -- Lista de Exercícios 2
maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maximo (maiorVenda (n-1)) (vendas n)

--Exercício 3 -- Lista de Exercícios 2
