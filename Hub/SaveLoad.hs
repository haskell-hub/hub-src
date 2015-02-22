--
-- >>> Hub.SaveLoad <<<
--
-- This module saves the hub configuration in an archive type and loads
-- an archive file, performing some analysis on the target Hub to be loaded
-- (if there is one).
--
-- (c) 2011-2012 Chris Dornan


module Hub.SaveLoad
    ( save
    , PkgDiffs(..)
    , load
    ) where

import           Data.Char
import qualified Data.Map               as Map
import           Data.List
import           Hub.Oops
import           Hub.Hub
import           Hub.Directory
import           Hub.Discover
import           Hub.PackageDB


-- Generates the archive on the standard output.

save :: Hub -> IO ()
save hub =
     do pdb <- packageDB hub
        let cts = unlines $ hdr : map (prettyPkgNick . iden2nick) (Map.keys pdb)
        putStr cts
      where
        hdr = "^=" ++ maybe (name__HUB hub) glb_hnUHB (usr___HUB hub)

data PkgDiffs = PD { hubPD :: Hub, surPD, msgPD, allPD :: [PkgNick] }
                                                                deriving (Show)

-- Takes the name of the hub to be loaded, the existing hub (if it exists)
-- an a flag indicating whether the hub is bing loaded or verified, and
-- reads in the archive from the standard input and returns the new
-- and the packages it is missing and the packages to be added.
--
--   * If it is not passed a hub then it creates a new one.
--
--   * If it is passed a hub which does not have the right global database
--     then it will remove and recreate the hub with the right global databse
--     if loading or generate an error if verifying.

load :: HubName -> Maybe Hub -> Bool -> IO PkgDiffs
load hn mb_hub0 vy =
     do mb_prs   <- parse_input
        (gh,nks) <- case mb_prs of
                       Nothing -> oops HubO $ "parse error"
                       Just pr -> return pr
        mb_hub <-
            case mb_hub0 of
              Nothing  -> return Nothing
              Just hub ->
                case usr___HUB hub of
                  Just uhb | gh==glb_hnUHB uhb -> return $ Just hub
                           | not vy            -> r_noth $ deleteHub hub
                  _                            -> oops HubO mm_msg
        g_hub <- discover $ Just gh
        hub   <- case mb_hub of
                   Nothing  -> createHub' False g_hub hn False False
                   Just hub -> return hub
        nks0 <- (map iden2nick . Map.keys) `fmap` packageDB hub
        return $ PD hub (nks0\\nks) (nks\\nks0) nks
      where
        r_noth = fmap (const Nothing)
        mm_msg = "global hub mismatch"

parse_input :: IO (Maybe (HubName,[PkgNick]))
parse_input =
     do cts <- getContents
        return $ parse_har cts

parse_har :: String -> Maybe (HubName,[PkgNick])
parse_har cts =
     do (gh,r) <- hdr_p $ pp cts
        (,) gh `fmap` bdy_p r

hdr_p :: [String] -> Maybe (HubName,[String])
hdr_p []       = Nothing
hdr_p (ln:lns) =
        case words ln of
          ['^':'=':hn]
            | isHubName hn == Just GlbHK
                -> Just (hn,lns)
          _     -> Nothing

bdy_p :: [String] -> Maybe [PkgNick]
bdy_p []       = Just []
bdy_p (ln:lns) =
        case words ln of
          [w] | Just nk <- parsePkgNick' w ->
             do nks <- bdy_p lns
                return $ nk:nks
          _ ->  Nothing

pp :: String -> [String]
pp = filter (not . all isSpace) . map sc . lines
      where
        sc ln = foldr cmt "" ln

        cmt '#' _ = ""
        cmt '-' t = case t of
                      '-':_ -> ""
                      _     -> '-':t
        cmt c   t = c:t
