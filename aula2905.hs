data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
 deriving(Eq, Show)

arv :: Arvore Int
arv = Nodo 10 (Nodo 14 (Folha 15) (Folha 14)) (Nodo 3 (Folha 7) (Folha 8))


somaArvore :: Arvore Int -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- 1.  Defina uma funcao que multiplique por 2 os inteiros em uma arvore.

multiplicaPorDois :: Arvore Int -> Arvore Int
multiplicaPorDois (Folha n) = Folha (n * 2)
multiplicaPorDois (Nodo n a1 a2) = Nodo (n * 2) (multiplicaPorDois a1) (multiplicaPorDois a2)

-- 2.  Defina uma funçao que conte quantos elementos existem em uma arvore

qtdeElementosArvore :: Arvore Int -> Int
qtdeElementosArvore (Folha n) = 1
qtdeElementosArvore (Nodo n a1 a2) = 1 + (qtdeElementosArvore a1) + (qtdeElementosArvore a2)

-- 3.  Defina uma funcao que ache o maior elemento de uma arvore

maiorElementoArvore :: Arvore Int -> Int
maiorElementoArvore (Folha n) = n
maiorElementoArvore (Nodo n a1 a2) = maximoArvore (maximoArvore n (maiorElementoArvore a1)) (maximoArvore n (maiorElementoArvore a2))

maximoArvore :: Int -> Int -> Int
maximoArvore x y 
 | x > y = x
 | otherwise = y

-- 4.  Defina a funcao que diz se um inteiro ocorre dentro de uma arvore
