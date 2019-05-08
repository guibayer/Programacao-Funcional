somaTripla :: [(Int,Int,Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):xs) = a + b + c + somaTripla xs

somaTupla :: [((Int,Int),(Int,Int))] -> Int
somaTupla [] = 0
somaTupla(((a,b),(c,d)):xs) = a + b + c + d + somaTupla xs

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] (y:ys) = []
zipp (x:xs) [] = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

zipp3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
zipp3 x y [] = []
zipp3 x [] z = []
zipp3 [] y z = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

unZippEsq :: [(Int,Int)] -> [Int]
unZippEsq [] = []
unZippEsq ((a,b):xs) = a : unZippEsq xs

unZippDir :: [(Int,Int)] -> [Int]
unZippDir [] = []
unZippDir ((a,b):ys) = b : unZippDir ys

unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp l = (unZippEsq l, unZippDir l)