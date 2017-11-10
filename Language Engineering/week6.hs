import Data.Monoid

{- class Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

   Nothing | Just a

  instance Applicative Maybe where
    pure :: a -> Maybe a
    pure x = Just x
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (Just f) <*> (Just x) = Just (f x)
    Nothing <*> Nothing = Nothing
    (Just f) <*> Nothing = Nothing
    Nothing <*> (Just x) = Nothing

pure id <*>

<*> :: Maybe
-}

data List a = Empty | Cons a (List a) deriving (Eq, Ord, Show)

instance Monoid (List a)
 where
   mempty = Empty
   Empty `mappend` y = y
   Cons x xs `mappend` ys = Cons x (xs <> ys)

instance Functor List where
  --fmap :: (a -> b) -> List a -> List b
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

(<$) :: a -> List b -> List a
x <$ Empty = Empty
x <$ Cons y ys = Cons x (fmap (const x) ys)
{-
instance Applicative List where
  --pure :: a -> List a
  --(<*>) :: List (a -> b) -> List a -> List b
  pure x = Cons x Empty
  --(Cons f fs) <*> (Cons x xs) = ()
-}
