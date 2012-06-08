--
-- >>> Hub.FilePaths <<<
--
-- This module abstracts the layout of /usr/hs -- the system hub area.
--
-- (c) 2011-2012 Chris Dornan


module Hub.FilePaths where

import           Text.Printf
import           Text.Regex
import           Data.Maybe


hubLib, sysVersion, distroDefaultHubPath, sysDefaultHubPath,
                    defaultHubPath, globalHubDir,
                    toolsBin, hubGccBin, hubBinutilsBin :: FilePath

hcBinREs, globalHubREs, hpDbREs :: String

hubLib                  = "/usr/hs/lib"
sysVersion              = "/usr/hs/lib/version.txt"
distroDefaultHubPath    = "/usr/hs/lib/distro-default.hub"
sysDefaultHubPath       = "/usr/hs/lib/sys-default.hub"
defaultHubPath          = "/usr/hs/lib/the-default.hub"
globalHubDir            = "/usr/hs/hub"
toolsBin                = "/usr/hs/tools"
hubGccBin               = "/usr/hs/gcc/bin"
hubBinutilsBin          = "/usr/hs/binutils/bin"

hcBinREs                = "/usr/hs/ghc/([a-zA-Z0-9_.-]+)/bin"
globalHubREs            = "/usr/hs/db/([^/]+)\\.d"
hpDbREs                 = "/usr/hs/db/("++hp_re++")(\\.d)?"


hp_re :: String
hp_re = "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9](-[a-z0-9]*)?"



--
-- Regular Expression Utilities
--


mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

gmatch :: Regex -> String -> Bool
gmatch re st = isJust $ matchRegex re st

match :: Regex -> String -> Maybe String
match re st = case matchRegex re st of
                Just (se:_) -> Just se
                _           -> Nothing
