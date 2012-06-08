--
-- >>> Version <<<
--
-- Keeper of the version information.
--
-- (c) 2011-2012 Chris Dornan


module Version
    ( version
    , main
    ) where


version :: String
version = "1.0.0"


main :: IO ()
main = putStrLn version
