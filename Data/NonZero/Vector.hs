{-# LANGUAGE DataKinds
            ,GADTs
            ,KindSignatures
            ,RankNTypes
            ,TemplateHaskell
            ,TypeOperators
            #-}
{-|
Module     : Data.NonZero.Vector
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Non-empty Vectors, indexed by their length.
-}
module Data.NonZero.Vector
  (Vector(Only,(:|))
  ,singleton

  ,head
  ,tail
  ,last

  ,length
  ,lengthNatural

  ,snoc
  ,append,(++)

  ,index

  ,mapVector
  ,mapMVector

  ,replicate
  ,replicateA
  ,replicateM

  ,toList
  ,fromList
  ,fromNatural

  ,foldl
  ,foldr

  ,concat
  ,take
  ,drop

  ,elem
  ,notElem

  ,zip
  ) where

import Prelude hiding (replicate,last,head,tail,map,length
                      ,append,(++),foldl,foldr,concat,take
                      ,drop,elem,notElem,zip)

import Data.NonZero.Natural

import Control.Applicative

-- | A Vector contains one or many elements and the number of elements is reflected
-- in the type.
data Vector (n :: Nat) a where

  Only :: a -> Vector One a

  (:|) :: a -> Vector n a -> Vector (Suc n) a

infixr 5 :|

instance Functor (Vector n) where
  fmap = mapVector

-- | A 'Vector' of size 1.
-- / The last element of a 'Vector'.
singleton = Only :: a -> Vector One a

-- | Safely extract the head element of a 'Vector'.
head :: Vector n a -> a
head (Only a)  = a
head (a :| _) = a

-- | Safely extract the tail of a 'Vector'.
tail :: Vector (Suc n) a -> Vector n a
tail (a :| as) = as

-- | Extract the last element of a 'Vector'.
last :: Vector n a -> a
last (Only a)  = a
last (a :| as) = last as

-- | Length of 'Vector' as an 'Int'.
length :: Vector n a -> Int
length (Only a)  = 1
length (a :| as) = 1 + length as

-- | Length of 'Vector' as a 'Natural'.
lengthNatural :: Vector n a -> Natural n
lengthNatural (Only a)   = One
lengthNatural ( a :| as) = Suc (lengthNatural as)

-- | snoc a single element to the end of a 'Vector'.
snoc :: Vector n a -> a -> Vector (Suc n) a
snoc (Only a)  x = a :| singleton x
snoc (a :| as) x = a :| (snoc as x)

-- | Append one 'Vector' to another.
append :: Vector n a -> Vector m a -> Vector (n :+: m) a
append (Only a)  bs = a :| bs
append (a :| as) bs = a :| (as `append` bs)

(++) :: Vector n a -> Vector m a -> Vector (n :+: m) a
infixr 5 ++
(++) = append

-- | Index an element from a 'Vector'.
index :: (m :<=: n) => Vector n a -> Natural m -> a
index (Only a)  One     = a
index (a :| _)  One     = a
index (_ :| as) (Suc n) = as `index` n

-- | Map a function over a 'Vector' == fmap.
mapVector :: (a -> b) -> Vector n a -> Vector n b
mapVector = fmap

-- | Map a monadic function over the elements of a 'Vector'.
mapMVector :: Monad m => (a -> m b) -> Vector n a -> m (Vector n b)
mapMVector f (Only a)    = f a >>= return . Only
mapMVector f (a :| as) = do
  b  <- f a
  bs <- mapMVector f as
  return $ b :| bs


-- | Replicate a 'Vector' of n* elements of 'a'.
replicate :: Natural n -> a -> Vector n a
replicate One     a = Only a
replicate (Suc n) a = a :| (n `replicate` a)

-- | Replicate a vector of n* elements from some 'Applicative m => m a'.
replicateA :: Applicative m => Natural n -> m a -> m (Vector n a)
replicateA One     ma = Only <$> ma
replicateA (Suc n) ma = (:|) <$> ma <*> n `replicateA` ma

-- | Replicate a vector of n* elements from some 'Monad m => m a'.
replicateM :: Monad m => Natural n -> m a -> m (Vector n a)
replicateM One ma = ma >>= return . Only
replicateM (Suc n) ma = do
  a  <- ma
  as <- n `replicateM` ma
  return $ a :| as


-- | Convert a 'Vector' to a list.
toList :: Vector n a -> [a]
toList (Only a)    = [a]
toList (a :| as) = a : toList as

-- | Build a 'Vector' from a list, equal in length to the given 'Natural'.
fromList :: [a] -> Natural n -> Maybe (Vector n a)
fromList []     _       = Nothing
fromList [a]    One     = Just $ Only a
fromList (a:as) (Suc n) = (a :|) <$> (fromList as n)

-- | Build a 'Vector' of size n, using a function to map each index to an element.
fromNatural :: (Int -> a) -> Natural n -> Vector n a
fromNatural f n = fromNatural' 1 f n
  where
    fromNatural' :: Int -> (Int -> a) -> Natural n -> Vector n a
    fromNatural' i f One     = Only (f i)
    fromNatural' i f (Suc n) = (f i) :| (fromNatural' (i+1) f n)

-- | Left fold.
foldl :: (a -> a -> a) -> Vector n a -> a
foldl _ (Only a)  = a
foldl f (a :| as) = foldl' f a as
  where
    foldl' :: (a -> b -> a) -> a -> Vector n b -> a
    foldl' f a (Only b)  = f a b
    foldl' f a (b :| bs) = foldl' f (f a b) bs

-- | Right fold.
foldr :: (a -> a -> a) -> Vector n a -> a
foldr _ (Only a)  = a
foldr f (a :| as) = f a (foldr f as)

-- | Concatenate a 'Vector' of 'Vectors'.
concat :: Vector n (Vector m a) -> Vector (n :*: m) a
concat (Only bs) = bs
concat (b :| bs) = b `append` (concat bs)

-- | Take a 'Vector' of the first 'n' elements from a larger (or equal) 'Vector' of length 'm'.
take :: (n :<=: m) => Natural n -> Vector m a -> Vector n a
take One     (Only a)  = Only a
take One     (a :| _)  = Only a
take (Suc n) (a :| as) = a :| (take n as)

-- | Drop the first 'n' elements from a 'Vector' of size 'm', producing a 'Vector' sized 'm - n'.
--
-- Note: Dropping 'One' from a 'Vector' with only one element will return the Vector unmodified.
drop :: (n :<=: m) => Natural n -> Vector m a -> Vector (m :-: n) a
drop One     (Only a)  = Only a
drop One     (a :| as) = as
drop (Suc n) (a :| as) = drop n as

-- | Test whether a given element is contained in the 'Vector'.
elem :: Eq a => Vector m a -> a -> Bool
elem (Only a)  x = a == x
elem (a :| as) x = (a == x) || (elem as x)

-- | Test whether a given element is not contained in the 'Vector'.
notElem :: Eq a => Vector m a -> a -> Bool
notElem (Only a)  x = a /= x
notElem (a :| as) x = (a /= x) && (notElem as x)

-- | Zip together two equally sized 'Vector's.
zip :: Vector n a -> Vector n b -> Vector n (a,b)
zip (Only a)  (Only b)  = Only (a,b)
zip (a :| as) (b :| bs) = (a,b) :| (zip as bs)

