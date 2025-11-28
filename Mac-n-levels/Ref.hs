{-# LANGUAGE Trustworthy #-}

-- | Mutuable state (references)
module MAC.Ref (
  MacRef,
  newMacRef,
  readMacRef,
  writeMacRef,
)
where

import           Data.IORef
import           Mac.NumLattice

-- Trustworthy
import           Mac.Mac

-- Unsafe
import           Mac.Effects

-- | Labeled references
type MacRef l a = Res l (IORef a)

-- | Creation of labeled references
newMacRef :: (Leq l l') => a -> Mac l (MacRef l' a)
newMacRef = newTCB . newIORef

-- | Reading labeled references
readMacRef :: (Leq l' l) => MacRef l' a -> Mac l a
readMacRef = readTCB readIORef

-- | Writing labeled references
writeMacRef :: (Leq l l') => MacRef l' a -> a -> Mac l ()
writeMacRef secref v = writeTCB (`writeIORef` v) secref

