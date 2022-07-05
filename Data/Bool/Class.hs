{- |
Module: Data.Bool.Class
Description: Generalizations of 'Bool'
Copyright: ⓒ 2022 Anselm Schüler
License: MIT
-}

module Data.Bool.Class (
  Boolean(..),
  BooleanIf(..),
  boolean
) where

-- | Generalization of boolean operators
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
  false _ = false
  true _ = true
  (&&*) f g x = f x &&* g x
  (||*) f g x = f x ||* g x
  notB f x = notB $ f x

-- | 'Boolean's that can be evaluated
class Boolean b => BooleanIf b where
  bIf :: b -> a -> a -> a

instance BooleanIf Bool where
  bIf a b c = if a then b else c

-- | Generalization of 'Data.Bool.bool' using 'bIf'
boolean :: BooleanIf b => a -> a -> b -> a
boolean a b c = bIf c a b
