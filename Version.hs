--
-- >>> Version <<<
--
-- Keeper of the version information.
--
-- (c) 2011-2015 Chris Dornan


module Version
    ( version
    , main
    ) where


version :: String
version = "1.9.0"


main :: IO ()
main = putStrLn version
