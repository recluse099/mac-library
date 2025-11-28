{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Mac.NumLattice (
  Leq,
  L0,
  L1,
  L2,
) where

import           Data.Kind          (Constraint)
import           Data.Proxy
import           Data.Type.Equality
import           GHC.TypeLits

type Leq (a :: Nat) (b :: Nat) = (CmpNat a b == 'GT) ~ 'False

-- | Example label type synonyms
type L0 = 0 -- lowest

type L1 = 1
type L2 = 2 -- highest (for 3-level example)
