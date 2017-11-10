{-# LANGUAGE RankNTypes #-}
--1.1

data Fix f = In(f(Fix f))

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = (alg.fmap(cata alg).inOp)

inOp :: (Fix f) -> (f (Fix f))
inOp (In(k)) = k

--1.2

data Robot = Right | Left | Forward Int (Robot) | Stop

data RobotF k = RightF k | LeftF k | ForwardF Int k | StopF

instance Functor RobotF where
  --fmap :: (a -> b) -> RobotF -> b
  fmap f StopF = StopF
  fmap f (RightF k) = RightF (f k)
  fmap f (LeftF k) = LeftF (f k)
  fmap f (ForwardF x k) = ForwardF x (f k)

--obeying the functor laws:

--CASE STOP
--(1) mapping id on stop creates just the function
--case 1: fmap (id) Stop = Stop (no interaction with the function)

--(2) mapping (f.g) on the functor is the same as mapping f then g, aka f(g())
--case 1: fmap (f.g) Stop =  Stop             (stop doesnt interact with the function)
--       fmap f (fmap g(Stop)) = fmap f(Stop) = Stop

--CASE RightF
--(1) mapping id on RightF creates just the function
--case 2: fmap (id) RightF = RightF (no interaction with the function)

--(2) mapping (f.g) on the functor is the same as mapping f then g, aka f(g())
--case 2: fmap (f.g) RightF =  RightF             (stop doesnt interact with the function)
--       fmap f (fmap g(RightF)) = fmap f(RightF) = RightF

--CASE LeftF
--(1) mapping id on LeftF creates just the function
--case 3: fmap (id) LeftF = LeftF (no interaction with the function)

--(2) mapping (f.g) on the functor is the same as mapping f then g, aka f(g())
--case 4: fmap (f.g) LeftF =  LeftF             (stop doesnt interact with the function)
--       fmap f (fmap g(LeftF)) = fmap f(LeftF) = LeftF

--CASE ForwardF
--(1) mapping id on forward creates just the function
--case 1: fmap (id) (ForwardF i j) = ForwardF i (id j) = ForwardF i j

--(2) mapping (f.g) on the functor is the same as mapping f then g, aka f(g())
--case1: fmap (f.g) (ForwardF i j) =  ForwardF i (f.g(j)) = ForwardF i (f(g(j)))            (stop doesnt interact with the function)
--       fmap f (fmap g(Forward i j)) = fmap f(Forward i (g j)) = Forward i (f(g(j)))



totalDist :: Fix(RobotF) -> Int
totalDist = cata algDistTotal where
  algDistTotal (LeftF k) = k
  algDistTotal (RightF k) = k
  algDistTotal (StopF) = 0
  algDistTotal (ForwardF i j) = i + j

distFacing :: Fix(RobotF) -> Int
distFacing x = fst (cata algFacing x)  where
  algFacing (LeftF (v, c)) = (v, c - 1)
  algFacing (RightF (v, c)) = (v, c + 1)
  algFacing (StopF) = (0,0)
  algFacing (ForwardF i (j, c)) = (if c == 0 then (i+j,c) else (j, c))

fir :: (a, a, a) -> a
fir (x, y, z) = x

thr :: (a, a, a) -> a
thr (x, y, z) = z

triangulate :: Int -> Int -> Float
triangulate x y = sqrt(fromIntegral (x*x) + fromIntegral(y*y))

distLine :: Fix(RobotF) -> Float
distLine x = triangulate (fir (cata algLine x)) (thr (cata algLine x)) where
  algLine (LeftF (v, c, hrz)) = (v, c - 1, hrz)
  algLine (RightF (v, c, hrz)) = (v, c + 1, hrz)
  algLine (StopF) = (0, 0, 0)
  algLine (ForwardF i (j, c, hrz))
      | c == 0 = (i+j, c, hrz)
      | c < 0 = (j, c, hrz - i)
      | c > 0 = (j, c, hrz + i)


--7
--(1) = (cata alg.h)
--(2) = (InOp.fmap. cata alg. fmap h. b)
--(3) = (InOp.fmap cata alg. alg. h)

--1.3

data ListK a k = EmptyK | ConsK a k

instance Functor (ListK a) where
  fmap f EmptyK = EmptyK
  fmap f (ConsK x xs) = ConsK x (f xs)

mcata :: Functor f => (forall x . (x -> a) -> (f x -> a)) -> Fix f -> a
mcata phi = (phi (mcata phi)) . inOp

lenAlg :: (x -> Int) -> (ListK a x -> Int)
lenAlg g EmptyK = 0
lenAlg g (ConsK x xs) = (g xs) + 1

sumAlg :: (x -> Int) -> (ListK Int x -> Int)
sumAlg g EmptyK = 0
sumAlg g (ConsK x xs) = (g xs) + x

sumList :: Fix(ListK Int) -> Int
sumList = mcata alg where
  alg = sumAlg
