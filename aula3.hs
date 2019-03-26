--Lista 3 Exercício 1
type TresInteiros = (Int, Int, Int)

teste :: TresInteiros
teste = (2, 2, 2)

adicionaTupla :: TresInteiros -> Int
adicionaTupla (a,b,c) = a + b + c

--Lista 3 Exercício 2
--Defina a funcao shift:
--shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift (n, t) = (t, n)

-- Exerc 3
minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax x y z
 | x > y && x > z = (1, 2)
