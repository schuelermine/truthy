{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module: Data.Bool.Truthy
Description: Values that can be treated as 'Boolean's
Copyright: ⓒ 2022 Anselm Schüler
License: MIT
This module provides the 'Truthy' class for values that can be treated as 'Boolean's,
as well as several newtypes that assign boolean interpretations to existing data.
-}

module Data.Bool.Truthy where

import Data.Bool.Class

-- | Values that can be treated as 'Boolean's
class Boolean (BooleanRep b) => Truthy b where
  -- | The 'Boolean' representation of the value
  type BooleanRep b
  -- | Convert the 'Truthy' value to its 'Boolean' representation
  truthy :: b -> BooleanRep b

instance Truthy Bool where
  type BooleanRep Bool = Bool
  truthy = id

instance Truthy b => Truthy (a -> b) where
  type BooleanRep (a -> b) = a -> BooleanRep b
  truthy = (truthy .)

-- | A wrapper for numbers ('Num' instances) that is converted by 'truthy' to 'False' if it is @0@
newtype ZeroFalse n = ZeroFalse { unZeroFalse :: n } deriving (Show, Eq, Ord, Num, Functor, Foldable, Traversable)

instance (Eq n, Num n) => Truthy (ZeroFalse n) where
  type BooleanRep (ZeroFalse n) = Bool
  truthy (ZeroFalse n) = n /= 0

-- | A wrapper for numbers ('Num' instances) that is converted by 'truthy' to 'True' if it is @> 0@
newtype PositiveTrue n = PositiveTrue { unPositiveTrue :: n } deriving (Show, Eq, Ord, Num, Functor, Foldable, Traversable)

instance (Ord n, Num n) => Truthy (PositiveTrue n) where
  type BooleanRep (PositiveTrue n) = Bool
  truthy (PositiveTrue n) = n > 0

-- | A wrapper for 'Foldable' data structures that is converted by 'truthy' to 'False' if it is empty
newtype EmptyFalse f x = EmptyFalse { unEmptyFalse :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Foldable f => Truthy (EmptyFalse f x) where
  type BooleanRep (EmptyFalse f x) = Bool
  truthy (EmptyFalse xs) = null xs

-- | A wrapper for 'Foldable' data structures that 'truthy' evaluates by '(&&*)'-ing all elements
newtype AllTrue f x = AllTrue { unAllTrue :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Foldable f, Boolean x) => Truthy (AllTrue f x) where
  type BooleanRep (AllTrue f x) = x
  truthy (AllTrue xs) = foldr (&&*) true xs

-- | A wrapper for 'Foldable' data structures that 'truthy' evaluates by '(&&*)'-ing all elements’ boolean representations
newtype AllTruthy f x = AllTruthy { unAllTruthy :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Functor f, Foldable f, Truthy x) => Truthy (AllTruthy f x) where
  type BooleanRep (AllTruthy f x) = BooleanRep x
  truthy (AllTruthy xs) = foldr (&&*) true $ truthy <$> xs

-- | A wrapper for 'Foldable' data structures that 'truthy' evaluates by '(||*)'-ing all elements
newtype AnyTrue f x = AnyTrue { unAnyTrue :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Foldable f, Boolean x) => Truthy (AnyTrue f x) where
  type BooleanRep (AnyTrue f x) = x
  truthy (AnyTrue xs) = foldr (||*) false xs

-- | A wrapper for 'Foldable' data structures that 'truthy' evaluates by '(||*)'-ing all elements’ boolean representations
newtype AnyTruthy f x = AnyTruthy { unAnyTruthy :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Functor f, Foldable f, Truthy x) => Truthy (AnyTruthy f x) where
  type BooleanRep (AnyTruthy f x) = BooleanRep x
  truthy (AnyTruthy xs) = foldr (||*) false $ truthy <$> xs

-- | A wrapper for 'Truthy' instances that inverts their 'truthy' value
newtype Contrary b = Contrary { unContrary :: b } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Truthy b => Truthy (Contrary b) where
  type BooleanRep (Contrary b) = BooleanRep b
  truthy (Contrary b) = notB $ truthy b
