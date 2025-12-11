{-# LANGUAGE Trustworthy #-}

module Examples.MacWget where

import           Mac.Core             (Mac, ioTCB)
import           Mac.Lattice

import           Data.ByteString.UTF8 (toString)
import           Network.HTTP.Simple  (getResponseBody, httpBS, parseRequest)

{-
  For simplicity, when wgetMac gets called with http://bob.evil as a domain, it
  will write the request to a file
-}
wgetMac :: String -> Mac L String
wgetMac s
   | take (length domain) s == domain = do
       ioTCB $ appendFile "leaks.txt" $ s ++ "\n"
       return "launch"
   | otherwise =
       ioTCB $ do
         toString . getResponseBody <$> do
           httpBS =<< parseRequest s
  where
    domain = "http://bob.evil"
