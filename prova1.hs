-- exercício 3.

potencia :: Int -> Int -> Int
potencia b 0 = 1
potencia b n = b * potencia b (n-1)

-- exercício 4:
-- zipp [2,3] [4,5]
-- [(2,4),(3,5)]

zipp :: [a] -> [b] -> [(a,b)]
zipp [] y = []
zipp x [] = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

-- exercício 5: 
-- pack "aaabbc"
-- ["aaa", "bb", "c"]
pegaIguais :: Char -> String -> String
pegaIguais c [] = []
pegaIguais c (x:xs)
 | c == x = x : pegaIguais x xs
 | otherwise = []

retiraIguais :: Char -> String -> String
retiraIguais c [] = []
retiraIguais c (x:xs)
 | c == x = retiraIguais c xs
 | otherwise = x:xs

pack :: String -> [String]
pack [] = []
pack (x:xs) = pegaIguais x (x:xs) : pegaIguais (retiraIguais x xs)
