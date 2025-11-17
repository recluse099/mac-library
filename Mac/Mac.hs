{-# LANGUAGE Trustworthy #-}

-- | A safe interface for the module @Core.hs@
module Mac.Mac (
    -- It comes from Core
    Res (),
    labelOf,
    -- Monad Mac
    Mac (),
    runMac,
    -- Auxiliary proxies
    fix,
)
where

import           Mac.Core

-- | To help the type-system
fix :: l -> Mac l ()
fix _l = return ()
