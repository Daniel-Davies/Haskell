holes :: Char -> Int
holes '4' = 1
holes '6' = 1
holes '8' = 2
holes '9' = 1
holes x = 0

holey :: [Char] -> [Char]
holey [] = []
holey (x:xs) = if holes x > 0 then x : holey xs else holey xs

holeSum :: [Char] -> Int
holeSum xs = sum(map toInt ((holey xs)))


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat xs)

toInt :: Char -> Int
toInt '0' = 0
toInt '1' = 1
toInt '2' = 2
toInt '3' = 3
toInt '4' = 4
toInt '5' = 5
toInt '6' = 6
toInt '7' = 7
toInt '8' = 8
toInt '9' = 9

interleave :: [a] -> [a] -> [a]
--interleave [] [] = []
interleave [] (ys) = ys
interleave (xs) [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
