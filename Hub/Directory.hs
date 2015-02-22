--
-- >>> Hub.Directory <<<
--
-- All user hubs are managed in ~/.hubrc, the 'directory'. This module tries
-- to abstract the messy details.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Directory
    ( initDirectory
    , userHubExists
    , userHubAvailable
    , hubExists
    , doesHubExist
    , isUserHub
    , userHubPath
    , globalHubPath
    , defaultGlobalHubName
    , lsHubs
    , bin2toolchain
    , db2platform
    , allocHub
    , createHub
    , createHub'
    , renameHub
    , deleteHub
    , swapHub
    , notLocked
    , defaultDirectoryPath
    , GCMode(..)
    , gcDefaultDirectory
    ) where


import           Control.Applicative
import qualified Control.Exception      as E
import           Control.Monad
import           Data.List
import qualified Data.Map               as Map
import           System.IO
import           System.FilePath
import           System.Directory
import           System.Environment
import           Text.Printf
import           Text.Regex
import           Hub.FilePaths
import           Hub.Oops
import           Hub.System
import           Hub.Prog
import           Hub.Hub
import           Hub.Parse
import           Hub.PackageDB


-- ensure the default directory structure in user's home directory
-- is initialised

initDirectory :: IO ()
initDirectory =
     do (hub,lib) <- user_hub_dirs
        createDirectoryIfMissing True hub
        createDirectoryIfMissing True lib


-- check that a hub name denotes a named user hub that either
-- exists (userHubExists) or or has not been created (userHubAvailable)

userHubExists, userHubAvailable :: HubName -> IO ()

userHubExists hn =
     do _ <- checkHubName [UsrHK] hn
        hubExists   hn

userHubAvailable hn =
     do _ <- checkHubName [UsrHK] hn
        iuh <- isUserHub hn
        case iuh of
          True  -> oops PrgO $ printf "%s: hub already in use" hn
          False -> return ()

-- check that the named hub exists



hubExists :: HubName -> IO ()
hubExists hn =
     do ok <- doesHubExist hn
        case ok of
          True  -> return ()
          False -> oops PrgO $ printf "%s: no such hub" hn

doesHubExist :: HubName -> IO Bool
doesHubExist hn =
     do hf <- case isHubName hn of
                Just GlbHK -> return $ globalHubPath hn
                Just UsrHK -> userHubPath hn
                Nothing    -> oops PrgO $ printf "%s: bad hub name"
        fileExists hf

-- test whether a user hub exists

isUserHub :: HubName -> IO Bool
isUserHub hn = userHubPath hn >>= fileExists

-- generate the path of the Hub XML config file file for a
-- user hub (userHubPath) and global hub (globalHubPath)

userHubPath :: HubName -> IO FilePath
userHubPath hn = (\(h_fp,_,_)->h_fp) `fmap` user_hub_paths hn

globalHubPath :: HubName -> FilePath
globalHubPath hn = globalHubDir </> hn2fp hn


-- get the default global hub name

defaultGlobalHubName :: IO HubName
defaultGlobalHubName = sel
        [ usr_df     -- user-spec  default
        , dro_df     -- O/S distro default (may not be installed)
        , sys_df     -- Hub system default (may not be installed)
        , lhp_df     -- looks like the latest H.P. (guess)
        , lst_df     -- lexigographical maximum
        ]
      where
        sel []        = oops PrgO "no global hubs!"
        sel (p:ps)    =
                 do ei <- tryIO $ trim `fmap` p
                    case ei of
                      Left  _  -> sel ps
                      Right hn ->
                         do ex <- fileExists $ globalHubPath hn
                            case ex of
                              True  -> return hn
                              False -> sel ps

        usr_df        = trim `fmap` readAFile defaultHubPath

        dro_df        = trim `fmap` readAFile distroDefaultHubPath

        sys_df        = trim `fmap` readAFile sysDefaultHubPath

        lhp_df        = mx_fr $ gmatch hp_hub_re

        lst_df        = mx_fr $ const True

        mx_fr  p      = (filter p `fmap` lsHubs [GlbHK]) >>= mx

        mx     []     = ioError $ userError "Hub.Hub: internal error"
        mx     (x:xs) = return  $ foldl max x xs

        hp_hub_re     = mk_re "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9]"

-- list hubs

lsHubs :: [HubKind] -> IO [HubName]
lsHubs hks = concat `fmap` mapM ls (sort hks)
      where
        ls GlbHK = ls_glb_hubs
        ls UsrHK = ls_usr_hubs

-- convert 'bin' and 'db' paths to corresponding toolchai8ns and platforms

bin2toolchain, db2platform :: FilePath -> Maybe String
bin2toolchain = match $ mk_re hcBinREs
db2platform   = match $ mk_re hpDbREs

-- allocate, create/copy, rename, delete and swap hubs

allocHub :: IO HubName
allocHub =
     do (hd,_) <- user_hub_dirs
        alloc `fmap` getDirectoryContents hd
      where
        alloc gns = printf "__h%03d" $
                        (1+) $ maximum $ -1:[ i | Just i<-map prs gns ]

        prs fp = case reverse fp of
                   'l':'m':'x':'.':t -> prs' $ reverse t
                   _                 -> Nothing


        prs' :: String -> Maybe Int
        prs' ('_':'_':'h':t) = readMB t
        prs' _               = Nothing

createHub :: Bool -> Hub -> HubName -> IO ()
createHub  cp hub0 hn = const () `fmap` createHub' cp hub0 hn False False

createHub' :: Bool -> Hub -> HubName -> Bool -> Bool -> IO Hub
createHub' cp hub0 hn sf xf =
     do userHubAvailable hn
        (h_fp,lib,db0) <- user_hub_paths hn
        db <- case xf of
          True  -> sandbox hub0
          False -> return db0
        createDirectoryIfMissing True lib
        case cp of
          True  ->
             do db_ <- hub_user_db hub0
                cpFileDir db_ db
          False ->
                when (not xf) $ pkg_init hub0 db
        dy <- defaultDirectoryPath
        let gh   = maybe (name__HUB hub0) id $ usr_ghHUB hub0
            lk   = lockedHUB hub0
            hub1 = hub0 { name__HUB = hn
                        , kind__HUB = UsrHK
                        , path__HUB = h_fp
                        , usr___HUB = Just $ UHB dy gh db lk
                        }
        hub <- defaultComment sf hub1
        dump   hub
        return hub

defaultComment :: Bool -> Hub -> IO Hub
defaultComment sf hub =
        case sf of
          True  ->
            do cwd <- getCurrentDirectory
               return $ hub { commntHUB = cwd }
          False ->
               return $ hub { commntHUB = ""  }

renameHub :: Hub -> HubName -> IO ()
renameHub hub0 hn =
     do not_global hub0
        notLocked  hub0
        lib0          <- hub_user_lib hub0
        (h_fp,lib,db) <- user_hub_paths hn
        mvFileDir lib0 lib
        removeFile $ path__HUB hub0
        let f uhb = uhb   { usr_dbUHB=db }
            hub   = hub0  { name__HUB=hn
                          , path__HUB=h_fp
                          , usr___HUB=fmap f $ usr___HUB hub0
                          }
        dump hub

deleteHub :: Hub -> IO ()
deleteHub hub =
     do not_global hub
        notLocked  hub
        (h_fp,lib,_) <- user_hub_paths $ name__HUB hub
        removeFile h_fp
        removeRF   lib

swapHub :: Hub -> Hub -> IO ()
swapHub hub1 hub2 =
     do _    <- not_global hub1
        _    <- not_global hub2
        (_,lib1,_) <- user_hub_paths (name__HUB hub1)
        (_,lib2,_) <- user_hub_paths (name__HUB hub2)
        let hub1' = hub1 {name__HUB=name__HUB hub2,path__HUB=path__HUB hub2,usr___HUB=usr___HUB hub2}
            hub2' = hub2 {name__HUB=name__HUB hub1,path__HUB=path__HUB hub1,usr___HUB=usr___HUB hub1}
        dump hub1'
        dump hub2'
        swap_files lib1 lib2

notLocked :: Hub -> IO ()
notLocked hub =
        case lockedHUB hub of
          True  -> oops HubO $ printf "%s: this hub is locked" $ name__HUB hub
          False -> return ()

-- return the path of the default directory

defaultDirectoryPath :: IO FilePath
defaultDirectoryPath = home >>= \hme -> return $ printf "%s/.hubrc" hme

heap_dir :: FilePath -> FilePath
heap_dir = printf "%s/.hubrc/heap"

-- GC the default directory

data GCMode = DebugGCM | VerboseGCM | QuietGCM
                                                                deriving (Show)

gcDefaultDirectory :: (Maybe HubName->IO Hub) -> GCMode -> IO ()
gcDefaultDirectory discover gcm =
     do hns  <- lsHubs [ hk | hk<-[minBound..maxBound], hk/=GlbHK ]
        hubs <- mapM discover $ map Just hns
        dirs <- concat `fmap` mapM importLibraryDirs hubs
        hdir <- heap_dir `fmap` home
        hcts <- getDirectoryContents hdir
        let hnds   = Map.fromList [ (nd,()) | Just nd <- map readMB hcts ]
            l_hnds = dirs2heap_node_set hdir dirs
            g_hnds = hnds `Map.difference` l_hnds
      --putStrLn $ printf "---hnds---\n%s\n----------\n\n" $ show hnds
      --putStrLn $ printf "--l_hnds--\n%s\n----------\n\n" $ show l_hnds
      --putStrLn $ printf "--g_hnds--\n%s\n----------\n\n" $ show g_hnds
      --putStrLn $ printf "---hdir---\n%s\n----------\n\n" $ show hdir
      --putStrLn $ printf "---dirs---\n%s\n----------\n\n" $ show dirs
        case gcm of
          DebugGCM   -> hPutStrLn stderr $ printf "GC: %s" $ unwords $ map show $ Map.keys g_hnds
          VerboseGCM -> hPutStrLn stderr $ printf "GC: %d nodes collected" $ Map.size g_hnds
          QuietGCM   -> return ()
        collect hdir $ map show $ Map.keys g_hnds



--
-- GC Helpers
--

collect :: FilePath -> [FilePath] -> IO ()
collect dir sdirs = mapM_ clct sdirs
      where
        clct  = case True of
                  True  -> stash
                  False -> trash

        stash sdir =
             do hme <- home
                createDirectoryIfMissing False $ garbage hme
                mvFileDir (dir</>sdir) (garbage hme</>sdir)

        trash sdir = removeR (dir</>sdir)

dirs2heap_node_set :: FilePath -> [FilePath] -> Map.Map Int ()
dirs2heap_node_set hdir dirs =
            Map.fromList [ (nd,()) | Just nd<-map (dir2heap_node hdir) dirs ]

dir2heap_node :: FilePath -> FilePath -> Maybe Int
dir2heap_node hdir dir =
        case hdir `isPrefixOf` dir of
          True  -> readMB nd_s
          False -> Nothing
      where
        nd_s = takeWhile (not . isPathSeparator) $
                      dropWhile isPathSeparator  $ drop (length hdir) dir

readMB :: Read a => String -> Maybe a
readMB str =
    case [ x | (x,t)<-reads str, ("","")<-lex t ] of
      [x] -> Just x
      _   -> Nothing



--
-- Directory Structure
--
-- (see also 'defaultDirectoryPath', 'allocateDefaultDirectory' and
-- gcDefaultDirectory above)
--


package_config :: FilePath
package_config = "package.config"

user_lib :: FilePath -> HubName -> FilePath
user_lib hme hn = printf "%s/.hubrc/lib/%s" hme hn

garbage :: FilePath -> FilePath
garbage = printf "%s/.hubrc/garbage"

db_re :: String -> Regex
db_re hme = mk_re $ printf "%s/.hubrc/lib/([^/]*)/%s/?" hme package_config

hn2fp :: HubName -> FilePath
hn2fp = (++ ".xml")

fp2hn :: FilePath -> Maybe HubName
fp2hn = match xml_fn_re

xml_fn_re :: Regex
xml_fn_re = mk_re "(.*)\\.xml"



--
-- Listing Hubs
--


ls_glb_hubs :: IO [HubName]
ls_glb_hubs = (sort . chk) `fmap` getDirectoryContents globalHubDir
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], isHubName hn==Just GlbHK ]

ls_usr_hubs :: IO [HubName]
ls_usr_hubs =
     do dp <- fst `fmap` user_hub_dirs
        (sort . chk) `fmap` getDirectoryContents dp
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], isHubName hn==Just UsrHK ]



--
-- invoking ghc-pkg
--

pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
        execP HubO (EE InheritRS InheritRS [] []) FullMDE hub Ghc_pkgP ["init",fp]


user_hub_paths :: HubName -> IO (FilePath,FilePath,FilePath)
user_hub_paths hn =
     do (hub,lib) <- user_hub_dirs
        let h_l = lib </> hn
        return (hub </> hn2fp hn, h_l, h_l </> package_config)

user_hub_dirs :: IO (FilePath,FilePath)
user_hub_dirs =
     do hme <- home
        let hub = printf "%s/.hubrc/hub" hme
            lib = printf "%s/.hubrc/lib" hme
        return (hub,lib)

hub_user_lib :: Hub -> IO FilePath
hub_user_lib hub =
     do hme <- home
        db  <- hub_user_db hub
        case match (db_re hme) db of
          Just hn | hn==name__HUB hub
                -> return $ user_lib hme hn
          _     -> oops PrgO "hub has non-standard user-database path"

hub_user_db :: Hub -> IO FilePath
hub_user_db hub =
        case usr_dbHUB hub of
          Nothing -> oops PrgO "no user DB speceified for this hub"
          Just db -> return db


--
-- Swapping Files
--

swap_files :: FilePath -> FilePath -> IO ()
swap_files fp fp' = swap_files'' fp fp' $ oops PrgO

--swap_files' :: FilePath -> FilePath -> IO () -> IO ()
--swap_files' fp fp' tdy = swap_files'' fp fp' $ \err -> tdy >> oops PrgO err

swap_files'' :: FilePath -> FilePath -> (String->IO ()) -> IO ()
swap_files'' fp fp' h = catchIO (sw_files fp fp') $ \_ -> h err
      where
        err = printf "Failed to swap %s and %s (permissions?)" fp fp'

sw_files :: FilePath -> FilePath -> IO ()
sw_files fp_1 fp_2 =
     do fp_t <- mk_tmp 0 fp_1
        mvFileDir fp_1 fp_t
        mvFileDir fp_2 fp_1
        mvFileDir fp_t fp_2

mk_tmp :: Int -> FilePath -> IO FilePath
mk_tmp i fp =
     do yup <- fileDirExists fp'
        case yup of
          True  -> mk_tmp (i+1) fp
          False -> return fp'
      where
        fp' = printf "%s-%d" fp i



--
-- Ensure Hub is not Global
--


not_global :: Hub -> IO ()
not_global hub = when (kind__HUB hub==GlbHK) $
                    oops HubO $ printf "%s: is a global hub"  $name__HUB hub



--
-- Get HOME Environment Variable
--


home :: IO FilePath
home = catchIO (getEnv "HOME") $ \_ -> return "/"



--
-- Creating a Cabal Sandbox
--

sandbox :: Hub -> IO FilePath
sandbox hub = do
  execP HubO (EE InheritRS InheritRS [] []) FullMDE hub CabalP ["sandbox","init"]
  cwd <- getCurrentDirectory
  dirs <- filter ("ghc" `isInfixOf`) <$> getDirectoryContents ".cabal-sandbox"
  case dirs of
    [dir] -> return $ cwd </> ".cabal-sandbox" </> dir
    _     -> error "no package database located in the sandbox"



--
-- 'try' and 'catch' specialised for IO
--


tryIO :: IO a -> IO (Either IOError a)
tryIO = E.try


catchIO :: IO a -> (IOError->IO a) -> IO a
catchIO = E.catch

