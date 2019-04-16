somaPares :: [(Int, Int)] -> Int
somaPares [] = 0
somaPares ((c,d):x) = c + d + somaPares x

somaTripla :: [(Int,Int,Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):x) = a + b + c + somaTripla x

somaTupla :: [((Int,Int),(Int,Int))] -> Int
somaTupla [] = 0
somaTupla (((a,b),(c,d)):x) = a + b + c + d + somaTupla x
