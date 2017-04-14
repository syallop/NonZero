{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleInstances
            ,KindSignatures
            ,MultiParamTypeClasses
            ,TemplateHaskell
            ,TypeFamilies
            ,TypeSynonymInstances
            ,TypeOperators
            ,UndecidableInstances
            #-}
{-|
Module     : Data.NonZero.Nat
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Represent non-zero natural numbers at the type level.

This module is separate from NonZero.Natural so that we
can avoid exporting unwanted Term-level 'One' and 'Suc's
which we must declare to get DataKinds to derive the wanted
Type-level constructors.

Ideally we would be able to write something like:

@
  kind Nat
      = One
      | Suc Nat
@

which is what we are simulating.
-}
module Data.NonZero.Nat
  (Nat -- Type/Kind
  ,One -- Type :: Nat
  ,Suc -- Type :: Nat

  ,Plus,(:+:)
  ,Mult,(:*:)

  ,Minus,(:-:)

  ,(:<=:)(..)

  ,toNat
  ) where

import Language.Haskell.TH

-- | 'Nat' is used as the *kind* of natural numbers excluding a zero.
--
-- E.G.
--
-- @
--
-- One
--
-- Suc One
--
-- Suc Suc One
--
-- @
--
-- Are all types of kind 'Nat'.
data Nat
  = One
  | Suc Nat

-- | @ One :: Nat @
type One = 'One

-- | @ Suc :: Nat -> Nat @
type Suc = 'Suc

-- | Addition of 'Nat's.
type family Plus (n :: Nat) (m :: Nat) :: Nat where
  Plus One     m = Suc m
  Plus (Suc n) m = Suc (Plus n m)
type n :+: m = Plus n m

type family Minus (n :: Nat) (m :: Nat) :: Nat where
  Minus (Suc n) (Suc m) = Minus n m
  Minus (Suc n) One     = n
  Minus One     One     = One
type n :-: m = Minus n m

type family Mult (n :: Nat) (m :: Nat) :: Nat where
  Mult One     m = m
  Mult (Suc n) m = Plus m (Mult n m)
type n :*: m = Mult n m

-- | Decide LessThanOrEqual between two Nats.
type family (n :: Nat) :<= (m :: Nat) :: Bool where
  One   :<= n     = True
  Suc n :<= One   = False
  Suc n :<= Suc m = n :<= m

-- | 'n' is less than or equal to 'm'.
type (n :: Nat) :<=: (m :: Nat) = (n :<= m) ~ True

-- | TemplateHaskell type for conveniently writing 'Nat' types.
--
-- E.G.
--
-- @ $(toNat 3) @ ~> @ :: Suc (Suc One) @
toNat :: Integer -> Q Type
toNat 1 = [t| 'One |]
toNat n = [t| 'Suc $(toNat (n-1)) |]

