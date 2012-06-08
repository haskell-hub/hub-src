--
-- >>> Hub.PackageDB <<<
--
-- Sometimes the hub has to tangle with the Package DB format (mostly hub
-- erase): this package provides the tools.
--
-- (c) 2011-2012 Chris Dornan


module Hub.PackageDB
    ( PkgNick(..)
    , PkgName
    , PkgVrsn
    , PkgHash
    , iden2nick
    , parsePkgNick
    , parsePkgNick'
    , prettyPkgNick
    , eraseClosure
    , importLibraryDirs
    , PkgIden(..)
    , Package(..)
    , packageDB

    , DepGraph(..)
    , dep_graph
    ) where

import           Data.Char
import           Data.List
import           Data.Array
import qualified Data.Map               as Map
import           Data.Graph.Inductive
import           Text.Printf
import           System.Directory
import           Hub.System
import           Hub.Oops
import           Hub.Prog
import           Hub.Hub



-- Package as identified on the CL

data PkgNick = PKN
    { namePKN :: PkgName
    , vrsnPKN :: Maybe PkgVrsn
    }                                                    deriving (Eq,Ord,Show)

-- Fully qualified package as referenced in a package configuration file.

data PkgIden = PKI
    { namePKI :: PkgName
    , vrsnPKI :: PkgVrsn
    , idenPKI :: PkgHash
    }                                                    deriving (Eq,Ord,Show)

type PkgName = String
type PkgVrsn = String
type PkgHash = String


iden2nick :: PkgIden -> PkgNick
iden2nick pki = PKN (namePKI pki) (Just $ vrsnPKI pki)


eraseClosure :: Hub -> [PkgNick] -> IO ([PkgNick],[PkgNick])
eraseClosure hub pkns =
     do pdb  <- packageDB hub
        pkis <- mapM (resolve_pkg_nick pdb) pkns
        return (map pki2pkn pkis,map pki2pkn $ erase_deps pdb pkis)

importLibraryDirs :: Hub -> IO [FilePath]
importLibraryDirs hub =
     do pdb  <- packageDB hub
        return $ usort $ concat $ map dirs $ Map.elems pdb
      where
        dirs pkg = import_dirsPKG pkg ++ library_dirsPKG pkg

parsePkgNick :: String -> IO PkgNick
parsePkgNick s = maybe fl return $ parsePkgNick' s
      where
        fl = oops HubO $ printf "%s: invalid package name" s

parsePkgNick' :: String -> Maybe PkgNick
parsePkgNick' s =
        case is_val of
          True  | is_vr     -> Just $ PKN nm (Just vr)
                | otherwise -> Just $ PKN s   Nothing
          False             -> Nothing
      where
        is_val      = case s of
                        []   -> False
                        c:cs -> fval_c c && all sval_c cs

        is_vr       = not (null nm) && not (null vr) && all vr_c vr

        nm          = reverse r_nm
        vr          = reverse r_vr

        r_nm        = cl_d rst2
        (r_vr,rst2) = break ('-'==) $ reverse s

        cl_d ('-':t) = t
        cl_d x       = x

        vr_c '.'     = True
        vr_c c       = isDigit c

        fval_c c     = isAlpha c || c=='_'
        sval_c c     = isAlpha c || isDigit c || c `elem` "-_.=+#~"

prettyPkgNick :: PkgNick -> String
prettyPkgNick pkn =
        case vrsnPKN pkn of
          Nothing -> namePKN pkn
          Just vr -> printf "%s-%s" (namePKN pkn) vr

pki2pkn :: PkgIden -> PkgNick
pki2pkn pki = PKN (namePKI pki) (Just(vrsnPKI pki))


--
-- Calculating the Erase Dependencies
--

erase_deps :: PackageDB -> [PkgIden] -> [PkgIden]
erase_deps pdb pkis = map v2pki $ (\\ vs) $ usort $ concat $ map (pre gr) vs
      where
        vs      = [ v | Just v<-map pki2mbv pkis ]
        pki2mbv = flip Map.lookup $ assgnDG dg
        v2pki   = (arrayDG dg!)
        gr      = trc $ graphDG dg
        dg      = dep_graph pdb

usort :: Ord a => [a] -> [a]
usort = foldr no_dups [] . sort
      where
        no_dups x []      = [x]
        no_dups x (x':xs) = if x==x' then x:xs else x:x':xs


--
-- Building the Dependency Graph
--

data DepGraph = DG
    { assgnDG :: Map.Map PkgIden Int
    , arrayDG :: Array Int PkgIden
    , graphDG :: Gr PkgIden ()
    }                                                           deriving (Show)

dep_graph :: PackageDB -> DepGraph
dep_graph pdb = DG asgn arry (mkGraph ndes edgs)
      where
        ndes = map (\(x,y)->(y,x)) $ Map.toList asgn

        edgs = [ (x',y',()) | (x,y)<-deps,
                        Just x'<-[Map.lookup x asgn],
                        Just y'<-[Map.lookup y asgn] ]

        deps = [ (idPKG pkg,dep) | pkg<-Map.elems pdb, dep<-dependsPKG pkg ]

        arry = listArray (0,Map.size asgn-1) $ Map.keys asgn

        asgn = Map.fromList $ zip (sort $ Map.keys pdb) [0..]


--
-- Resolving Package Nicks into Package Ids
--

resolve_pkg_nick :: PackageDB -> PkgNick -> IO PkgIden
resolve_pkg_nick pdb pkn =
        case match_pkg_nick pdb pkn of
          []    -> oops HubO $ printf "%s: does not match any user packages in the hub" $ prettyPkgNick pkn
          [pki] -> return pki
          _     -> oops HubO $ printf "%s: matches multiple user packages in the hub"   $ prettyPkgNick pkn

match_pkg_nick :: PackageDB -> PkgNick -> [PkgIden]
match_pkg_nick pdb pkn =
            [ pki | pki <- Map.keys pdb,
                        namePKN pkn==namePKI pki
                                    && maybe True (==vrsnPKI pki) (vrsnPKN pkn)]


--
-- Reconstructing the Package DB
--

type PackageDB = Map.Map PkgIden Package

data Package = PKG
    { idPKG           :: PkgIden
    , import_dirsPKG  :: [FilePath]
    , library_dirsPKG :: [FilePath]
    , dependsPKG      :: [PkgIden]
    }                                                           deriving (Show)

packageDB :: Hub -> IO PackageDB
packageDB hub =
     do cts <- package_dump hub
        return $ packages [ package $ record rec | rec<-records cts ]

package_dump :: Hub -> IO String
package_dump hub =
     do tf <- tmpFile "pkg-dump.txt"
        let ee = EE (RedirctRS tf) DiscardRS []
        execP HubO ee UserMDE hub Ghc_pkgP ["dump"]
        ct <- readAFile tf
        removeFile tf
        return ct

packages :: [Package] -> Map.Map PkgIden Package
packages pkgs = Map.fromList [(idPKG pkg,pkg) | pkg<-pkgs ]

package :: Map.Map String [String] -> Package
package mp = PKG
        (s2pr (lu "id"           sgl))
              (lu "import-dirs"  lst)
              (lu "library-dirs" lst)
              (lu "depends"      prs)
      where
        lu ky f = f $ Map.lookup ky mp

        sgl Nothing    = ""
        sgl (Just lns) = trim $ unlines lns

        lst Nothing    = []
        lst (Just lns) = map trim lns

        prs Nothing    = []
        prs (Just lns) = map (s2pr . trim) lns

s2pr :: String -> PkgIden
s2pr s = PKI (reverse r_nm) (reverse r_vr) (reverse r_hs)
      where
        r_nm        = cl_d rst2
        (r_vr,rst2) = break ('-'==) $ cl_d rst1
        (r_hs,rst1) = break ('-'==) $ reverse s

        cl_d ('-':t) = t
        cl_d x       = x

records :: String -> [[String]]
records cts = rec [] $ lines cts
      where
        rec acc []       = [ reverse acc | not $ null acc ]
        rec acc (ln:lns) =
                case ln of
                  ""            -> rec acc lns
                  _ | ln=="---" -> reverse acc:rec [] lns
                    | otherwise -> rec (ln:acc) lns

record :: [String] -> Map.Map String [String]
record lns = Map.fromList $ fields lns

fields :: [String] -> [(String,[String])]
fields []       = []
fields (ln:lns) = (tag,rst:cnt):fields lns'
      where
        (tag,rst)   = case words ln of
                        []   -> ("","")
                        w:ws -> (cln w,unwords ws)

        (cnt,lns')  = span chk lns

        chk []      = False
        chk (c:_) = isSpace c

        cln tg      = case reverse tg of
                        ':':r_tg -> reverse r_tg
                        _        -> tg


