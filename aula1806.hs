data Lista a = Vazia | Cons a (Lista a)
 deriving(Eq,Show)

lista1 :: Lista Int
lista1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Vazia))))

lista2 :: Lista (Lista Int) 
lista2 = Cons (Cons 1 (Cons 2 Vazia)) (Cons Vazia (Cons (Cons 4 Vazia) Vazia)) 

tamanho :: Lista Int -> Int 
tamanho Vazia = 0 
tamanho (Cons x xs) = 1 + tamanho xs

somaLista :: Lista Int -> Int
somaLista Vazia = 0
somaLista (Cons x xs) = x + somaLista xs


mapLista :: (a->b) -> Lista a -> Lista b
mapLista func Vazia = Vazia
mapLista func (Cons x xs) = Cons (func x) (mapLista func xs)

triplica :: Int -> Int
triplica x = x * 3

foldr1Lista :: (a -> a -> a) -> Lista a -> a
foldr1Lista f (Cons x Vazia) = x
foldr1Lista f (Cons x xs) = f x (foldr1Lista f xs)

somaLista2 :: Int -> Int -> Int
somaLista2 x y = x + y
