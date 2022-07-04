{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bool.Truthy where

class Boolean b where
  false :: b
  true :: b
  (&&*) :: b -> b -> b
  (||*) :: b -> b -> b
  notB :: b -> b

instance Boolean Bool where
  false = False
  true = True
  (&&*) = (&&)
  (||*) = (||)
  notB = not

instance Boolean b => Boolean (a -> b) where
  true _ = true
  false _ = false
  (&&*) f g x = f x &&* g x
  (||*) f g x = f x ||* g x
  notB f x = notB $ f x

class Boolean (BooleanRep b) => Truthy b where
  type BooleanRep b
  truthy :: b -> BooleanRep b

instance Truthy Bool where
  type BooleanRep Bool = Bool
  truthy = id

instance Truthy b => Truthy (a -> b) where
  type BooleanRep (a -> b) = a -> BooleanRep b
  truthy = (truthy .)

newtype ZeroFalse n = ZeroFalse { unZeroFalse :: n } deriving (Show, Eq, Ord, Num, Functor, Foldable, Traversable)

instance (Eq n, Num n) => Truthy (ZeroFalse n) where
  type BooleanRep (ZeroFalse n) = Bool
  truthy (ZeroFalse n) = n /= 0

newtype PositiveTrue n = PositiveTrue { unPositiveTrue :: n } deriving (Show, Eq, Ord, Num, Functor, Foldable, Traversable)

instance (Ord n, Num n) => Truthy (PositiveTrue n) where
  type BooleanRep (PositiveTrue n) = Bool
  truthy (PositiveTrue n) = n > 0

newtype EmptyFalse f x = EmptyFalse { unEmptyFalse :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Foldable f => Truthy (EmptyFalse f x) where
  type BooleanRep (EmptyFalse f x) = Bool
  truthy (EmptyFalse xs) = null xs

newtype AllTrue f x = AllTrue { unAllTrue :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Foldable f, Boolean x) => Truthy (AllTrue f x) where
  type BooleanRep (AllTrue f x) = x
  truthy (AllTrue xs) = foldr (&&*) true xs

newtype AllTruthy f x = AllTruthy { unAllTruthy :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Functor f, Foldable f, Truthy x) => Truthy (AllTruthy f x) where
  type BooleanRep (AllTruthy f x) = BooleanRep x
  truthy (AllTruthy xs) = foldr (&&*) true $ truthy <$> xs

newtype AnyTrue f x = AnyTrue { unAnyTrue :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Foldable f, Boolean x) => Truthy (AnyTrue f x) where
  type BooleanRep (AnyTrue f x) = x
  truthy (AnyTrue xs) = foldr (||*) false xs

newtype AnyTruthy f x = AnyTruthy { unAnyTruthy :: f x } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Functor f, Foldable f, Truthy x) => Truthy (AnyTruthy f x) where
  type BooleanRep (AnyTruthy f x) = BooleanRep x
  truthy (AnyTruthy xs) = foldr (||*) false $ truthy <$> xs

newtype Contrary b = Contrary { unContrary :: b } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Truthy b => Truthy (Contrary b) where
  type BooleanRep (Contrary b) = BooleanRep b
  truthy (Contrary b) = notB $ truthy b

class Boolean b => BooleanIf b where
  bIf :: b -> a -> a -> a

instance BooleanIf Bool where
  bIf a b c = if a then b else c

boolean :: BooleanIf b => a -> a -> b -> a
boolean a b c = bIf c a b
