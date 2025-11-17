{-# LANGUAGE Trustworthy #-}

-- | Mutuable state (references)
module MAC.Ref (
  MACRef,
  newMACRef,
  readMACRef,
  writeMACRef,
)
where

import           Data.IORef
import           Mac.Lattice

-- Trustworthy
import           Mac.Mac

-- Unsafe
import           Mac.Effects

-- | Labeled references
type MACRef l a = Res l (IORef a)

-- | Creation of labeled references
newMACRef :: (Less l l') => a -> MAC l (MACRef l' a)
newMACRef = newTCB . newIORef

-- | Reading labeled references
readMACRef :: (Less l' l) => MACRef l' a -> MAC l a
readMACRef = readTCB readIORef

-- | Writing labeled references
writeMACRef :: (Less l l') => MACRef l' a -> a -> MAC l ()
writeMACRef secref v = writeTCB (`writeIORef` v) secref

