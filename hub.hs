--
-- >>> Main <<<
--
-- Main driver for the hub tool (see the README for details).
--
-- (c) 2011-2012 Chris Dornan


module Main(main) where

import           Control.Monad
import           System.IO
import           System.Exit
import           Text.Printf
import           Hub.System
import           Hub.FilePaths
import           Hub.Directory
import           Hub.CommandLine
import           Hub.Commands
import qualified Version        as V


main :: IO ()
main =
     do cl <- commandLine
        case cl of
          HelpCL    _   _        -> return ()
          VrsnCL                 -> return ()
          _                      -> initDirectory
        case cl of
          ProgCL    hub (prg,as) -> _prog hub prg as
          HelpCL    err hlp      -> _help err hlp
          VrsnCL                 -> _vrsn
          DfltCL                 -> _default
          StDfCL    hub          -> _default_hub $ Just hub
          RsDfCL                 -> _default_hub   Nothing
          LsCL af qf             -> _ls af qf
          GetCL                  -> _get
          SetCL     hub          -> _set     hub
          UnsetCL                -> _unset
          NameCL    hub          -> _name    hub
          InfoCL    hub          -> _info    hub
          LockCL    hub          -> _lock    hub
          UnlockCL  hub          -> _unlock  hub
          PathCL    hub          -> _path    hub
          XmlCL     hub          -> _xml     hub
          InitCL    hub hn set   -> _init    hub hn set
          CommentCL hub    cmt   -> _comment hub cmt
          CpCL      hub hn       -> _cp      hub hn
          MvCL      hub hn       -> _mv      hub hn
          RmCL      hub          -> _rm      hub
          SwapCL    hub hn       -> _swap    hub hn
          GcCL                   -> _gc
          ListCL    hub          -> _list    hub
          CheckCL   hub          -> _check   hub
          SaveCL    hub          -> _save    hub
          LoadCL        hn       -> _load        hn
          VerifyCL  hub      sf  -> _verify  hub      sf
          InstallCL hub pkns     -> _install hub pkns
          EraseCL   hub pkns ef  -> _erase   hub pkns ef


_help :: Bool -> String -> IO ()
_help False hlp = putStr hlp
_help True  hlp = hPutStrLn stderr hlp >> exitWith (ExitFailure 1)

_vrsn :: IO ()
_vrsn =
     do putStr $ printf "hub %s\n" V.version
        ex <- fileExists sysVersion
        when ex $
            readAFile sysVersion >>= putStr


