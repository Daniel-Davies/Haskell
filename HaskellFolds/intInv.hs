{- Find the limits of the int type -}

n :: Int
n = 42

main :: IO ()
main = print n

foo :: Int -> Int
foo = if n < 0
      then n = n + 1
      else n

add' :: Int -> Int
add' n = if n > (n + 1)
         then foo n
         else add' (n * 2)
