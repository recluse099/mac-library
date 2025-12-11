{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Unsafe           #-}

module Examples.Alice1 where

import           Mac.Core
import           Mac.Labeled
import           Mac.Lattice

import           Data.Bits
import qualified Examples.Bob1 as Bob

{-
   Bob's code tries to exploit leaking information by leveraging
   exceptions and joinMAC.
-}

password :: IO String
password = do
    putStr "Please, select your password:"
    pass <- getLine
    lbool <-
        runMAC $
            (label pass :: MAC L (Labeled H String))
                >>= Bob.commonPass -- Bob's untrusted code trying to exploit joinMAC
    let MkId b = unRes lbool
    if b
        then putStrLn "Your password is too common!" >> password
        else return pass
