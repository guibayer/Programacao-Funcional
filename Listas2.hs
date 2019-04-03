membro :: [Int] -> Int -> Bool
membro [] v = False
membro (a:x) v = a == v || membro x v

membroNum :: [Int] -> Int -> Int
membroNum [] v = 0
membroNum (a:x) v 
 | a == v = 1 + membroNum x v
 | otherwise = 0 + membroNum x v

membro2 :: [Int] -> Int -> Bool
membro2 (a:x) v = (membroNum (a:x) v) > 0
