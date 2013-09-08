--
-- >>> Hub.Commands <<<
--
-- This module provides the routines the Main dispatcher calls fater analysing
-- the CommandLine AST.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Commands
    ( _prog
    , _default
    , _default_hub
    , _ls
    , _get
    , _set
    , _unset
    , _name
    , _info
    , _lock
    , _unlock
    , _path
    , _xml
    , _init
    , _comment
    , _cp
    , _mv
    , _rm
    , _swap
    , _gc
    , _list
    , _check
    , _load
    , _save
    , _verify
    , _install
    , _erase
    ) where

import           Data.Char
import           Control.Monad
import           System.FilePath
import           System.Directory
import           Text.Printf
import           Hub.FilePaths
import           Hub.Oops
import           Hub.System
import           Hub.Prog
import           Hub.Hub
import           Hub.PackageDB
import           Hub.Directory
import           Hub.Parse
import           Hub.Discover
import           Hub.SaveLoad



_prog :: Hub -> Prog -> [String] -> IO ()
_prog hub prog as = execProg HubO (EE InheritRS InheritRS [] ftr) FullMDE hub prog as
  where
    ftr = hub_ftr_env $ enmPROG prog

_default :: IO ()
_default = defaultGlobalHubName >>= putStrLn

_default_hub :: Maybe Hub -> IO ()
_default_hub Nothing   = fileExists defaultHubPath >>= \ex -> when ex $ removeFile defaultHubPath
_default_hub (Just hub) = is_global hub >> writeAFile defaultHubPath (name__HUB hub)

is_global :: Hub -> IO ()
is_global hub = when (kind__HUB hub/=GlbHK) $
                    oops HubO $ printf "%s: not a global hub" $name__HUB hub


_ls :: Bool -> Bool -> IO ()
_ls af qf =
     do hns  <- lsHubs [UsrHK,GlbHK]
        hubs <- mapM (discover . Just) $ filter chk hns
        putStr $ unlines $ [ fmt nm co lk | hub<-hubs,
                                let nm = name__HUB hub,
                                let co = commntHUB hub,
                                let lk = if lockedHUB hub then "L" else " " ]
      where
        chk ('_':'_':_) = af
        chk _           = True

        fmt nm co lk  = case qf of
                          True  -> nm
                          False -> printf "%-20s %s -- %s" nm lk co

_get :: IO ()
_get =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  ->
             do hn <- trim `fmap` readAFile ".hub"
                _ <- checkHubName [UsrHK,GlbHK] hn
                putStrLn hn

_set :: Hub -> IO ()
_set hub = writeAFile ".hub" $ name__HUB hub ++ "\n"

_unset ::   IO ()
_unset =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  -> removeFile ".hub"

_name :: Hub -> IO ()
_name hub = putStrLn $ name__HUB hub

_info :: Hub -> IO ()
_info hub = putStr $ unlines $
    [ printf "%-23s%s"                     (name++hs++lk) cmt                                    ] ++
    [ printf "      GHC              : %s" hc                | Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "      Haskell Platform : %s" hp                | Just hp <- [db2platform   glb_db] ] ++
    [ printf "      Tools            : %s"         hc_bin                                        ] ++
    [ printf "      Cabal Install    : %s"         cv        | Just cv <- [ci_vrn]               ] ++
    [        "      Package DBs"                                                                 ] ++
    [ printf "          global       : %-50s (%s)" glb_db gh                                     ] ++
    [ printf "          user         : %-50s (%s)" usr_db uh | Just usr_db <- [mb_ud], True      ]
  where
    hs     = case sourceHUB hub of
               ClHS -> " "
               EvHS -> " [ENV] "
               DrHS -> " [DIR] "
               DsHS -> " [SYS] "
    lk     = case lockedHUB hub of
               True  -> "[LOCKED] "
               False -> " "
    gh     = case usr_ghHUB hub of
               Nothing -> name
               Just hn -> hn
    uh     = name
    cmt    = if null cmt0 then "" else "-- " ++ cmt0
    name   = name__HUB hub
    cmt0   = commntHUB hub
    hc_bin = hc_binHUB hub
    ci_vrn = ci_vrnHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_lock :: Hub -> IO ()
_lock hub = lock True hub

_unlock :: Hub -> IO ()
_unlock hub = lock False hub

lock :: Bool -> Hub -> IO ()
lock lck hub =
        case usr_dbHUB hub of
          Nothing -> oops HubO msg
          Just dr ->
             do cs <- filter isc `fmap` getDirectoryContents dr
                lockFileDir True lck dr
                mapM_ (lockFileDir False lck) [dr</>c|c<-cs]
                dump lk_hub
      where
        isc fp  = case reverse fp of
                    'f':'n':'o':'c':'.':_ -> True
                    'e':'h':'c':'a':'c':_ -> True
                    _                     -> False

        lk_hub  = hub { usr___HUB = fmap lk' $ usr___HUB hub }

        lk' uhb = uhb { lockedUHB = lck }

        msg     = printf "%s: cannot (un)lock a global hub" $ name__HUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub = readAFile (path__HUB hub) >>= putStr

_init :: Hub -> HubName -> Bool -> IO ()
_init hub0 hn sf =
     do hub <- createHub' False hub0 hn sf
        when sf $ _set hub

_comment :: Hub -> String -> IO ()
_comment hub cmt = dump $ hub { commntHUB = cmt }

_cp :: Hub -> HubName -> IO ()
_cp hub hn = createHub True hub hn

_mv :: Hub -> HubName -> IO ()
_mv hub hn = renameHub hub hn

_rm :: Hub -> IO ()
_rm = deleteHub

_swap :: Hub -> HubName -> IO ()
_swap hub1 hn2 =
     do hub2 <- discover $ Just hn2
        swapHub hub1 hub2

_gc :: IO ()
_gc = gcDefaultDirectory discover VerboseGCM

_list :: Hub -> IO ()
_list hub = execP HubO (EE InheritRS InheritRS [] []) FullMDE hub Ghc_pkgP ["list"]

_check :: Hub -> IO ()
_check hub = execP HubO (EE InheritRS InheritRS [] []) FullMDE hub Ghc_pkgP ["check"]

_save :: Hub -> IO ()
_save = save

_load :: HubName -> IO ()
_load hn =
     do _   <- checkHubName [UsrHK] hn
        thr <- doesHubExist         hn
        mb  <- case thr of
                 True  -> Just `fmap` discover (Just hn)
                 False -> return Nothing
        pd  <- load hn mb False
        let hub = hubPD pd
            sps = surPD pd
            mps = msgPD pd
            aps = allPD pd
        case sps of
          []  -> return ()
          _:_ -> _erase hub sps True
        case sps++mps of
          []  -> return ()
          _:_ -> _install hub aps

_verify :: Hub -> Bool -> IO ()
_verify hub sf =
     do _  <- checkHubName [UsrHK] $ name__HUB hub
        pd <- load (name__HUB hub) (Just hub) True
        let sps = surPD pd
            mps = msgPD pd
        case (sf,sps) of
          (True,_:_) -> oops_sm "surplus" sps
          _          -> return ()
        case mps of
          []  -> return ()
          _:_ -> oops_sm "missing" mps
      where
        oops_sm adj ps = oops HubO $ adj ++ " packages: " ++
                                                unwords (map prettyPkgNick ps)

_install :: Hub -> [PkgNick] -> IO ()
_install hub pkns =
     do notLocked hub
        execP HubO (EE InheritRS InheritRS [] []) FullMDE hub CabalP
                                            ("install":map prettyPkgNick pkns)

_erase :: Hub -> [PkgNick] -> Bool -> IO ()
_erase hub pkns0 ff =
     do notLocked hub
        (pkns,d_pkns) <- eraseClosure hub pkns0
        putStr "Packages requested to be deleted:\n"
        putStr $ unlines $ map fmt pkns
        putStr "Dependent packages also to be deleted:\n"
        putStr $ unlines $ map fmt d_pkns
        go <-   case ff of
                  True  ->
                        return True
                  False ->
                     do putStr "Would you like to delete these packages? [n]\n"
                        yn <- getLine
                        return $ map toLower yn `elem` ["y","yes"]
        case go of
          True  ->
             do mapM_ unreg $ pkns ++ d_pkns
                putStr "package(s) deleted.\n"
          False -> putStrLn "No modules deleted."
      where
        fmt   pkn = "  " ++ prettyPkgNick pkn

        unreg pkn = execP HubO (EE InheritRS DiscardRS [] []) UserMDE hub
                                    Ghc_pkgP ["unregister",prettyPkgNick pkn]
