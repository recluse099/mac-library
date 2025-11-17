{-# LANGUAGE Unsafe #-}

-- | It provides functions which map read and write effects into security checks.
module Mac.Effects (
  newTCB,
  writeTCB,
  readTCB,
)
where

import           Mac.Core
import           Mac.Lattice

{- |
    It lifts functions which create resources into secure functions which
    create labeled resources
-}
newTCB :: (Less l l') => IO (d a) -> Mac l (Res l' (d a))
newTCB io = ioTCB io >>= return . MkRes

{- |
    It lifts an 'IO'-action which writes into a data type @d a@
    into a secure function which writes into a labeled resource
-}
writeTCB :: (Less l l') => (d a -> IO ()) -> Res l' (d a) -> Mac l ()
writeTCB io (MkRes a) = ioTCB $ io a

{- |
    It lifts an 'IO'-action which reads from a data type @d a@
    into a secure function which reads from a labeled resource
-}
readTCB :: (Less l' l) => (d a -> IO a) -> Res l' (d a) -> Mac l a
readTCB io (MkRes da) = ioTCB $ io da
