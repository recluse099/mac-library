{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Bob2 where

import           Data.List
import           Data.Maybe

import           Mac.Control
import           Mac.Labeled
import           Mac.Lattice
import           Mac.Mac
import           Mac.Ref

import           Control.Monad
import           Data.List.Split

-- Bob's code
commonPass ::
    (String -> Mac L String) ->
    Labeled H String ->
    Mac L (Labeled H Bool)
commonPass wget lpass = do
    str <- wget "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    let lines = filter (not . null) (linesBy (== '\n') str)
    let words = filter (not . (== '#') . head) lines
    joinMac $ do
        pass <- unlabel lpass
        return $ isJust $ find (== pass) words

-- Memoization
memMac :: (String -> Mac L String) -> Mac L (String -> Mac L String)
memMac f = newMacRef (100, []) >>= (return . cacheMac f)

cacheMac ::
    (String -> Mac L String) ->
    MacRef L (Int, [(String, String)]) ->
    String ->
    Mac L String
cacheMac f ref str = do
    (n, _) <- readMacRef ref
    when (n == 0) $ writeMacRef ref (100, [])
    (n, map) <- readMacRef ref
    case find (\(i, o) -> i == str) map of
        Nothing -> do
            result <- f str
            writeMacRef ref (n - 1, (str, result) : map)
            return result
        Just (_, o) -> do
            writeMacRef ref (n - 1, map)
            return o
