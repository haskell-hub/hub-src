--
-- >>> Main (prep) <<<
--
-- This program 'Haskelises' the hub help text.
--
-- (c) 2011-2012 Chris Dornan


module Main(main) where

import System.Locale
import Data.Time
import Text.Printf


main :: IO ()
main =
     do cts <- readFile "help.txt"
        writeFile "Hub/HelpText.hs" $ mk_text_mod  "Hub.HelpText" "helpText" cts

mk_text_mod :: String -> String -> String -> String
mk_text_mod mn fn cts =
        case lines cts of
          []     -> error "that is strange, the text file is empty"
          ln:lns -> unlines $ pre ln ++ foldr mdl pst lns
      where
        pre ln   = [ printf "module %s(%s) where" mn fn
                   ,        ""
                   , printf "%s :: String"        fn
                   , printf "%s = unlines"        fn
                   , printf "    [ %s"     $ show ln
                   ]

        mdl ln t = [ printf "    , %s"     $ show ln
                   ] ++ t

        pst      = [        "    ]"
                   ]
