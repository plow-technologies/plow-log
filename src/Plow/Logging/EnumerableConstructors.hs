{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Plow.Logging.EnumerableConstructors (HasEnumerableConstructors (..)) where

import Data.Proxy
import GHC.Generics

class HasEnumerableConstructors1 f where
  listConstructors1 :: f p -> [String]
  allConstructors1 :: Proxy f -> [String]

instance HasEnumerableConstructors1 V1 where
  listConstructors1 _ = []
  allConstructors1 _ = []

instance HasEnumerableConstructors1 U1 where
  listConstructors1 _ = []
  allConstructors1 _ = []

instance (HasEnumerableConstructors a) => HasEnumerableConstructors1 (K1 i a) where
  listConstructors1 (K1 x) = listConstructors x
  allConstructors1 _ = allConstructors (Proxy :: Proxy a)

instance HasEnumerableConstructors1 f => HasEnumerableConstructors1 (D1 c f) where
  listConstructors1 (M1 x) = listConstructors1 x
  allConstructors1 _ = allConstructors1 (Proxy :: Proxy f)

instance (Constructor c, HasEnumerableConstructors1 f) => HasEnumerableConstructors1 (C1 c f) where
  listConstructors1 x@(M1 y) = [conName x] ++ listConstructors1 y
  allConstructors1 _ = [conName (undefined :: C1 c f g)] ++ allConstructors1 (Proxy :: Proxy f)

instance HasEnumerableConstructors1 f => HasEnumerableConstructors1 (S1 c f) where
  listConstructors1 (M1 x) = listConstructors1 x
  allConstructors1 _ = allConstructors1 (Proxy :: Proxy f)

instance (HasEnumerableConstructors1 a, HasEnumerableConstructors1 b) => HasEnumerableConstructors1 (a :+: b) where
  listConstructors1 (L1 x) = listConstructors1 x
  listConstructors1 (R1 x) = listConstructors1 x

  allConstructors1 _ = (allConstructors1 (Proxy :: Proxy a) ++ (allConstructors1 (Proxy :: Proxy b)))

instance (HasEnumerableConstructors1 a, HasEnumerableConstructors1 b) => HasEnumerableConstructors1 (a :*: b) where
  listConstructors1 (a :*: b) = listConstructors1 a ++ listConstructors1 b
  allConstructors1 _ = (allConstructors1 (Proxy :: Proxy a) ++ (allConstructors1 (Proxy :: Proxy b)))

instance HasEnumerableConstructors1 f => HasEnumerableConstructors1 (Rec1 f) where
  listConstructors1 (Rec1 a) = listConstructors1 a
  allConstructors1 _ = allConstructors1 (Proxy :: Proxy f)

class HasEnumerableConstructors a where
  listConstructors :: a -> [String]
  allConstructors :: Proxy a -> [String]
  default listConstructors :: (Generic a, HasEnumerableConstructors1 (Rep a)) => a -> [String]
  listConstructors = listConstructors1 . from

  default allConstructors :: (Generic a, HasEnumerableConstructors1 (Rep a)) => Proxy a -> [String]
  allConstructors _ = allConstructors1 (Proxy :: Proxy (Rep a))

instance {-# OVERLAPPABLE #-} HasEnumerableConstructors a where
  listConstructors _ = []
  allConstructors _ = []

instance HasEnumerableConstructors a => HasEnumerableConstructors [a] where
  listConstructors = concatMap listConstructors
  allConstructors _ = allConstructors (Proxy :: Proxy a)

instance HasEnumerableConstructors a => HasEnumerableConstructors (Maybe a) where
  listConstructors = maybe [] listConstructors
  allConstructors _ = allConstructors (Proxy :: Proxy a)

instance (HasEnumerableConstructors a, HasEnumerableConstructors b) => HasEnumerableConstructors (Either a b) where
  listConstructors = either listConstructors listConstructors
  allConstructors _ = allConstructors (Proxy :: Proxy a) ++ allConstructors (Proxy :: Proxy b)
