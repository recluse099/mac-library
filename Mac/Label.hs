{-# LANGUAGE Trustworthy #-}

-- | Labeled expressions.
module MAC.Label (
  Labeled (),
  Id (MkId, unId),
  label,
  unlabel,
)
where

import           Mac.Core    (Mac, Res)
import           Mac.Effects
import           Mac.Lattice

-- | Type denoting values of type @a@
newtype Id a = MkId {unId :: a}

-- | Labeled expressions
type Labeled l a = Res l (Id a)

-- | Creation of labeled expressions
label :: (Less l l') => a -> Mac l (Labeled l' a)
label = newTCB . return . MkId

-- | Observing labeled expressions
unlabel :: (Less l' l) => Labeled l' a -> Mac l a
unlabel = readTCB (return . unId)
