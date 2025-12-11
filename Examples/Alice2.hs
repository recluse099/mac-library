{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Unsafe           #-}

module Examples.Alice2 where

import           Examples.MacWget
import           Mac.Core
import           Mac.Labeled
import           Mac.Lattice

import           Data.Bits

import qualified Examples.Bob2    as Bob

{-
   Safe use of references. The password mananger uses memoization
   of Bob's function common_pass.
-}

password :: IO String
password = do
    wgetMem <- runMac $ Bob.memMac wgetMac
    try wgetMem

try wget = do
    putStr "Please, select your password:"
    pass <- getLine
    lbool <- runMac $ Bob.commonPass wget =<< (label pass :: Mac L (Labeled H String))
    let MkId b = unRes lbool
    if b
        then do
            putStrLn "Your password is too common!"
            try wget
        else return pass
