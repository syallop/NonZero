{-# LANGUAGE GADTs
            ,TemplateHaskell
            #-}
{-|
Module     : Data.NonZero.Natural
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Represent non-zero natural numbers whose terms have one-to-one correspondance with their
type - they're singletons.
-}
module Data.NonZero.Natural
  (Nat
  ,One
  ,Suc
  ,Plus(),(:+:),(:<=:)
  ,Mult(),(:*:)
  ,Minus(),(:-:)

  ,Natural(One,Suc)
  ,toNatural

  ,toNat
  ) where

import Data.NonZero.Nat

import Language.Haskell.TH

-- | Type of natural numbers excluding zero.
-- Each term uniquely determines (and is determined by a Nat type).
--
-- E.G.
--
-- @
--   One :: Natural One
--
--   Suc One :: Natural (Suc One)
--
--   Suc (Suc One) :: Natural (Suc (Suc One))
-- @
data Natural n where
  One :: Natural One
  Suc :: Natural n -> Natural (Suc n)

-- | TemplateHaskell expression for conveniently writing 'Natural' terms.
--
-- E.G.
--
-- @ $(toNatural 3) @ ~> @ Suc (Suc One) :: Natural (Suc (Suc One)) @
toNatural :: Integer -> Q Exp
toNatural 1 = [e| One |]
toNatural n = [e| Suc $(toNatural (n-1)) |]

