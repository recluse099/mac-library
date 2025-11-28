{-# LANGUAGE Unsafe #-}

{- | It defines main data structures for security, i.e., monad family 'Mac' and
  labeled resources 'Res'.
-}
module Mac.Core (
  -- Resources definitions
  Res (MkRes, unRes),
  labelOf,
  -- Monad MAC
  Mac (MkMac),
  runMac,
  -- IO actions into Mac
  ioTCB,
)
where

import           Control.Applicative

-- | Labeling expressions of type @a@ with label @l@.
newtype Res l a = MkRes {unRes :: a}

-- | Label of resources
labelOf :: Res l a -> l
labelOf res = undefined

{- |
    This monad labels the results of the computation (of type @a@) with
    label @l@.
-}

-- newtype Mac l a = MkMac {unMac :: IO a}
newtype Mac l a = MkMac (IO a)

instance Functor (Mac l) where
  fmap :: (a -> b) -> Mac l a -> Mac l b
  fmap f (MkMac io) = MkMac (fmap f io)

instance Applicative (Mac l) where
  pure :: a -> Mac l a
  pure = MkMac . return
  (<*>) (MkMac f) (MkMac a) = MkMac (f <*> a)

instance Monad (Mac l) where
  return = pure
  (>>=) :: Mac l a -> (a -> Mac l b) -> Mac l b
  MkMac m >>= k = ioTCB (m >>= runMac . k)

-- | It lifts arbitrary 'IO'-actions.
ioTCB :: IO a -> Mac l a
ioTCB = MkMac

-- Should not be exported!

-- | Execute secure computations.
runMac :: Mac l a -> IO a
runMac (MkMac m) = m
