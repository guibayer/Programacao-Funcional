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

estaNaArvore :: Int -> Arvore Int -> Bool
estaNaArvore x (Folha n) = eIgual x n
estaNaArvore x (Nodo n a1 a2) = eIgual x n || estaNaArvore x a1 || estaNaArvore x a2

eIgual :: Int -> Int -> Bool
eIgual x y
 | x == y = True
 | otherwise = False



-- 5.  Defina uma funcao que diz quantas vezes um inteiro ocorre dentro de uma arvore

qtdeVezesQueOcorreu :: Int -> Arvore Int -> Int
qtdeVezesQueOcorreu x (Folha n) = eIgualInt x n
qtdeVezesQueOcorreu x (Nodo n a1 a2) = eIgualInt x n + qtdeVezesQueOcorreu x a1 + qtdeVezesQueOcorreu x a2

eIgualInt :: Int -> Int -> Int
eIgualInt x y
 | x == y = 1
 | otherwise = 0

-- 6.  Uma arvore  refletida   e  uma   arvore  com  seus  ramos  esquerdos  e  direitostrocados. 
-- Defina uma funcao refleteArvore

refleteArvore :: Arvore Int -> Arvore Int
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1) 

-- 7.  Defina uma funcao que calcule a altura de uma arvore

alturaDaArvore :: Arvore Int -> Int
alturaDaArvore (Folha n) = 1 
alturaDaArvore (Nodo n a1 a2) = (maximoArvore (alturaDaArvore a1) (alturaDaArvore a2))+1

-- 8.  Defina uma funcao que transfore uma arvore em uma lista

transformaArvoreEmLista :: Arvore Int -> [Int]
transformaArvoreEmLista (Folha n) = [n] 
transformaArvoreEmLista (Nodo n a1 a2) = (n : (transformaArvoreEmLista a1)) ++ (transformaArvoreEmLista a2)

-- 9.  Defina a funcao mapTree que aplica uma funcao a todos os inteiros de todosos nos de uma arvore.

mapTree :: (a -> b) -> Arvore a -> Arvore b
