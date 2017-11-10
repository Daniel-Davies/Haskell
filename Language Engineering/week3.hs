data Fix f = In(f(Fix f))

data TreeF a k = LeafF | BranchF a k k

data Tree a = Leaf | Branch a (Tree a) (Tree a)

instance Functor (TreeF a) where
  fmap f LeafF = LeafF
  fmap f (BranchF a c k) = (BranchF a (f c) (f k))

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Branch x l r) = x + (sumTree l) + (sumTree r)

numLeaves :: Tree Int -> Int
numLeaves Leaf = 1
numLeaves (Branch x l r) = (numLeaves l) + (numLeaves r)

inOp :: (Fix f) -> (f (Fix f))
inOp (In(k)) = k

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = (alg.fmap(cata alg).inOp)

sumTreeF :: (Fix (TreeF Int)) -> Int
sumTreeF = cata algSum
  where
    algSum (LeafF) = 0
    algSum (BranchF a k c) = a + k + c

leafTreeF :: (Fix (TreeF Int)) -> Int
leafTreeF = cata leafSumAlg
  where
    leafSumAlg (LeafF) = 1
    leafSumAlg (BranchF a k c) = k + c
