{-# LANGUAGE Trustworthy #-}

{- | Provide primitives to communicate among family members. It provides an
  API for the sequential 'joinMac'
-}
module Mac.Control (
    joinMac -- Secure communication for sequential programs
)
where

import           Mac.Core          (Mac, Res (MkRes), ioTCB, runMac)
import           Mac.Exception
import           Mac.Label
import           Mac.NumLattice

import           Control.Exception

{- |
   Primitive which allows family members to safely communicate. The function
   finishes even if an exception is raised---the exception is rethrown when
   the returned value gets inspected.
   __This function must not be used in a concurrent setting__.
-}
joinMac :: (Leq l l') => Mac l' a -> Mac l (Labeled l' a) -- safe joinMac
joinMac m =
    (ioTCB . runMac)
        (catchMac (m >>= safe_label) hd)
  where
    safe_label = return . MkRes . MkId
    hd = safe_label . throw . proxy
    proxy :: SomeException -> SomeException
    proxy = id

{-
  Note:
  Instead of safe_label, it is possible to use the primitive label. In that
  manner, we do not break abstraction and we have more confidence about the
  correctness of the implementation. However, by doing so, the type signature
  needs to add Less l' l' into the type constraints.  Since that constraint
  always hold, it can be show that m >>= label and label (throw e) is equivalent
  to m >>= safe_label and safe_label (throw e) in joinMAC.
-}


