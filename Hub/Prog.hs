--
-- >>> Hub.Prog <<<
--
-- This module records the other programs that hub is managing -- generally
-- the programs that come with a Haskell Plaform: the GHC tools, cabal and
-- a few others.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Prog
    ( Prog(..)
    , ProgType(..)
    , P(..)
    , p2prog
    , progMap
    ) where

import qualified Data.Map       as Map


data Prog = PROG
    { enmPROG :: P
    , nmePROG :: String
    , typPROG :: ProgType
    }                                                           deriving (Show)

data ProgType = HcPT | TlPT
                                                                deriving (Show)
data P
    = GhcP
    | GhciP
    | Ghc_pkgP
    | Hp2psP
    | HpcP
    | Hsc2hsP
    | RunghcP
    | RunhaskellP
    | CabalP
    | AlexP
    | HappyP
    | HaddockP
    | GhcModP
    | GhcModiP
    | HLintP
    | HDevToolsP
    | HaskellDocsP
    | StylishHaskellP
                                            deriving (Eq,Ord,Bounded,Enum,Show)

p2prog :: P -> Prog
p2prog p =
    case p of
      GhcP               -> PROG p "ghc"              HcPT
      GhciP              -> PROG p "ghci"             HcPT
      Ghc_pkgP           -> PROG p "ghc-pkg"          HcPT
      HaddockP           -> PROG p "haddock"          HcPT
      Hp2psP             -> PROG p "hp2ps"            HcPT
      HpcP               -> PROG p "hpc"              HcPT
      Hsc2hsP            -> PROG p "hsc2hs"           HcPT
      RunghcP            -> PROG p "runghc"           HcPT
      RunhaskellP        -> PROG p "runhaskell"       HcPT
      CabalP             -> PROG p "cabal"            TlPT
      AlexP              -> PROG p "alex"             TlPT
      HappyP             -> PROG p "happy"            TlPT
      GhcModP            -> PROG p "ghc-mod"          TlPT
      GhcModiP           -> PROG p "ghc-modi"         TlPT
      HLintP             -> PROG p "hlint"            TlPT
      HDevToolsP         -> PROG p "hdevtools"        TlPT
      HaskellDocsP       -> PROG p "haskell-docs"     TlPT
      StylishHaskellP    -> PROG p "stylish-haskell"  TlPT

progMap :: Map.Map String Prog
progMap = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]
