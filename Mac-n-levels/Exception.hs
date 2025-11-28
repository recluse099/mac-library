{-# LANGUAGE Trustworthy #-}

-- | lifting catch and throw to handle exceptions
module Mac.Exception (
    throwMac,
    catchMac,
)
where

import           Control.Exception
import           Mac.Core          (Mac (MkMac), ioTCB, runMac)

{- |
   Throwing exceptions securely
-}
throwMac :: (Exception e) => e -> Mac l a
throwMac = ioTCB . throw

{- |
   Throwing and catching exceptions are done among family members with the
   same labels
-}
catchMac :: (Exception e) => Mac l a -> (e -> Mac l a) -> Mac l a
catchMac (MkMac io) hd = ioTCB $ catch io (runMac . hd)
