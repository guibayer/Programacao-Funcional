osTresSaoIguais :: Int -> Int -> Int -> Bool
osTresSaoIguais x y z = ((x == y) && (y == z) && (z == x))

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z v = ((x == y) && (y == z) && (z == v) && (v == x)) 

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z
 | osTresSaoIguais x y z = 3
 | (x == y) || (y == z) = 2
 | otherwise = 0

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = ((x /= y) && (y /= z) && (z /= x))

-- 6. Defina a funcao todosIguais :: Int -> Int -> Int -> Bool

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = ((x == y) && (y == z) && (z == x))

-- 7. Escreva uma definicao de quantosSaoIguais que use a fun¸c˜ao todosDiferentes e a fun¸c˜ao todosIguais

quantosSaoIguais2::Int -> Int -> Int -> Int
quantosSaoIguais2 x y z
 | todosIguais x y z = 3
 | (x == y) || (y == z) = 2
 | todosDiferentes x y z = 0

-- 8. Defina a funcao elevadoDois :: Int -> Int que recebe um argumento n e devolve como resposta n 2

elevadoDois :: Int -> Int
elevadoDois x = x * x 

-- 9. Defina a funcao elevadoQuatro :: Int -> Int que recebe um argumento n e devolve como resposta n 4 .
-- Use elevadoDois para definir elevadoQuatro

elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois (elevadoDois x)

-- 10. Supondo que exista uma funcao vendas: vendas :: Int -> Int que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
-- na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente uma fun¸c˜ao chamada vendaTotal, que recebe um argumento n e calcula
-- todas as vendas da semana 0 at´e a semana n. Observe que essa fun¸c˜ao deve ser recursiva. Exemplo de calculo: As vendas da semana 0 at´e a semana 2,
-- podem ser calculados usando a seguinte formula: 
-- vendas 0 + vendas 1 + vendas 2

vendas :: Int -> Int
vendas 0 = 5
vendas 1 = 4 
vendas 2 = 3
vendas _ = 0

vendaTotal :: Int -> Int
vendaTotal x
 | x == 0 = vendas 0
 | otherwise = vendaTotal (x - 1) + vendas x