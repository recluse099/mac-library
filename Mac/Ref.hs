{-# LANGUAGE Trustworthy #-}

-- | Mutuable state (references)
module Mac.Ref (
  MacRef,
  newMacRef,
  readMacRef,
  writeMacRef,
)
where

import           Data.IORef
import           Mac.Lattice

-- Trustworthy
import           Mac.Mac

-- Unsafe
import           Mac.Effects

-- | Labeled references
type MacRef l a = Res l (IORef a)

-- | Creation of labeled references
newMacRef :: (Less l l') => a -> Mac l (MacRef l' a)
newMacRef = newTCB . newIORef

-- | Reading labeled references
readMacRef :: (Less l' l) => MacRef l' a -> Mac l a
readMacRef = readTCB readIORef

-- | Writing labeled references
writeMacRef :: (Less l l') => MacRef l' a -> a -> Mac l ()
writeMacRef secref v = writeTCB (`writeIORef` v) secref

