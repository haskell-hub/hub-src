--
-- >>> Hub.Help <<<
--
-- This module provides an interface to the help text 'help.txt',
-- Haskell-encoded in Hub.HelpText.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Help
    ( help
    , helpText
    , usage
    ) where

import           Data.Maybe
import           Text.Printf
import           Text.Regex
import           Hub.HelpText
import           Hub.Oops


help :: String -> IO String
help ""          = return helpText
help "--usage"   = dd_help `fmap` help "usage"
help "--help"    = dd_help `fmap` help "help"
help "--version" = dd_help `fmap` help "version"
help cd =
        case sc_help cd $ lines helpText of
          Nothing  -> oops HubO $ printf "%s: hub command not recognised" cd
          Just hlp -> return hlp

dd_help :: String -> String
dd_help ('h':'u':'b':' ':t) = "hub --" ++ t
dd_help hlp                 = hlp

sc_help :: String -> [String] -> Maybe String
sc_help _  []       = Nothing
sc_help cd (ln:lns) =
        case is_help_hdr cd ln of
          True  -> Just $ unlines $ ln : hdr ++ takeWhile is_help_bdy lns'
          False -> sc_help cd lns
      where
        (hdr,lns') = span (is_help_hdr cd) lns

is_help_hdr :: String -> String -> Bool
is_help_hdr cmd = match $ mk_re $ printf "hub %s.*" cmd

is_help_bdy :: String -> Bool
is_help_bdy = not . match not_help_bd_re

not_help_bd_re :: Regex
not_help_bd_re = mk_re "hub.*"



usage :: String
usage = unlines $ filter is_hdr $ lines helpText
      where
        is_hdr ('h':'u':'b':' ':_) = True
        is_hdr _                   = False


mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

match :: Regex -> String -> Bool
match re = isJust . matchRegex re
