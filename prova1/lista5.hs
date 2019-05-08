membro :: [Int] -> Int -> Bool
membro [] v = False
membro (x:xs) v
 | x == v = True
 | otherwise = membro xs v

membroNum :: [Int] -> Int -> Int
membroNum [] v = 0
membroNum (x:xs) v
 | x == v = 1 + membroNum xs v
 | otherwise = 0 + membroNum xs v

membro2 :: [Int] -> Int -> Bool
membro2 [] v = False
membro2 x v 
 | membroNum x v > 0 = True
 | otherwise = False

unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
 | membroNum xs x > 0 = unico (delete x xs)
 | otherwise = x : unico xs

delete :: Int -> [Int] -> [Int]
delete n [] = []
delete n (x:xs)
 | n == x = delete n xs
 | otherwise = n : delete n xs

iSort :: [Int] -> Int -> Bool
iSort [] v = False
iSort (x:xs) v 
 | x > v = False
 | x == v = True
 | otherwise = iSort xs v