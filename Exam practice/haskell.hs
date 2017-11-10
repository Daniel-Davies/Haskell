dropIt :: Int -> [a] -> [a]
dropIt 0 xs = xs
dropIt n [] = []
dropIt n (x:xs) = dropIt (n-1) xs

repeatIt :: a -> [a]
repeatIt x = x : repeatIt x

rankingIt :: [(String, Int)] -> [(String, Int)]
rankingIt = foldr f k where
  k = []
  f (s, i) [] = [(s,i)]
  f (s, i) ((str,inti) : xs)
    | i >= inti = (s,i) : ((str, inti):xs)
    | otherwise = ((str, inti): f (s,i) xs)

fromTuple :: [(String, Int)] -> [String]
fromTuple [] = []
fromTuple ((s,i):xs) = s : fromTuple xs

ranking :: [(String, Int)] -> [String]
ranking [] = []
ranking xs = fromTuple (rankingIt xs)
