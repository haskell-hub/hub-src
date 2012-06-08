--
-- >>> Hub.Oops <<<
--
-- This module generates the error messages. It has to be careful with the
-- context as the user my have invokled 'ghc' or 'cabal' (say) only for the
-- hub tool, acting as a wrapper, to encounter a problem in determing the
-- right tool and context to invoke.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Oops
    ( Oops(..)
    , oops
    , trim
    ) where

import Data.Char
import System.IO
import System.Exit
import System.Environment
import System.FilePath
import Text.Printf


data Oops
    = HubO      -- the hub command itself was invoked explicitly
    | PrgO      -- some other context: probably ghc, cabal, etc
                                                                deriving (Show)

oops :: Oops -> String -> IO a
oops o0 msg =
     do pn <- getProgName
        as <- getArgs
        let o  = refineO (snd $ splitFileName pn) o0
            ar = case as of
                   a:_ -> a
                   _   -> "*no program*"
        hPutStr stderr $ err pn ar o
        exitWith $ ExitFailure 1
      where
        err pn ar o =
                case o of
                  HubO -> printf "%s %s: %s\n"             pn ar msg
                  PrgO -> printf "%s (hub wrapper):  %s\n" pn    msg

refineO :: String -> Oops -> Oops
refineO "hub" _ = HubO
refineO _     o = o

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
