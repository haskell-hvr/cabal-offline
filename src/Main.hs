module Main where

import           Cabal.Plan
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process

plan2pids :: PlanJson -> Maybe (Map.Map PkgId Sha256)
plan2pids plan = do
    let pids0 :: Map.Map PkgId (Maybe Sha256)
        pids0 = Map.fromList [ (uPId u, uSha256 u) | (_,u@Unit { uType = UnitTypeGlobal }) <- Map.toList (pjUnits plan) ]

    sequenceA pids0

main :: IO ()
main = do
    cabalPkgCacheDir <- getAppUserDataDirectory "cabal/packages/hackage.haskell.org"

    args <- getArgs

    plans <- case args of
      [] -> do (plan, _) <- findAndDecodePlanJson Nothing
               pure [plan]

      _ -> mapM decodePlanJson args

    pidss <- maybe (fail "the impossible happened!") pure $ mapM plan2pids plans
    let  pids  = mconcat pidss
         pids' = map (T.unpack . dispPkgId) (Map.keys pids)

    mapM print pids'

    putStrLn "pre-fetching packages..."
    callProcess "cabal" (["fetch", "--no-dependencies"] ++ pids')

    forceRm "sdist/repo"
    createDirectoryIfMissing True "sdist/repo/package"
    createDirectoryIfMissing True "sdist/repo/index.revised"

    unlessM (doesDirectoryExist "sdist/keys") $ do
      putStrLn "generating new set of keys"
      callProcess "hackage-repo-tool" [ "-v" , "create-keys", "--keys", "sdist/keys" ]

    forM_ (Map.toList pids) $ \(pi@(PkgId (PkgName pnt) pv),sh) -> do
      let tarfn = T.unpack (dispPkgId pi) <.> "tar.gz"
          pn = T.unpack pnt
          cacheTarFn = cabalPkgCacheDir </> pn </> (T.unpack (dispVer pv)) </> tarfn
          cabfnd = "./sdist/repo/index" </> pn </> (T.unpack (dispVer pv))
          cabfn = cabfnd </> pn <.> "cabal"

      unlessM (doesFileExist cacheTarFn) $ do
        callProcess "cabal" ["fetch", "--no-dependencies", T.unpack (dispPkgId pi)]

      -- print (tarfn, cacheTarFn)
      -- TODO: verify sha256
      BS.writeFile ("./sdist/repo/package" </> tarfn) =<< BS.readFile cacheTarFn

      -- createDirectoryIfMissing True cabfnd
      -- BS.writeFile cabfn mempty


    callProcess "hackage-repo-tool" [ "-v", "--expire-root", "99"
                                    , "bootstrap"
                                    , "--keys", "sdist/keys"
                                    , "--repo", "sdist/repo"
                                    ]

    forceRm "./tmp"

    callProcess "cabal" (["get", "--destdir=tmp"] ++ pids')


    forM_ (Map.toList pids) $ \(pi@(PkgId (PkgName pnt) pv),sh) -> do
      let tarfn = T.unpack (dispPkgId pi) <.> "tar.gz"
          pn = T.unpack pnt
          cacheTarFn = cabalPkgCacheDir </> pn </> (T.unpack (dispVer pv)) </> tarfn
          cabfnd = "./sdist/repo/index" </> pn </> (T.unpack (dispVer pv))
          cabfn = cabfnd </> pn <.> "cabal"
          cabfnd2 = "./sdist/repo/index.revised" </> pn </> (T.unpack (dispVer pv))
          cabfn2 = cabfnd2 </> pn <.> "cabal"
          cabfn0 = "./tmp" </> T.unpack (dispPkgId pi) </> pn <.> "cabal"

      unlessM (doesFileExist cabfn0) $ do
        callProcess "cabal" ["get", "--destdir=tmp", T.unpack (dispPkgId pi)]

      cab0 <- BS.readFile cabfn0
      cab1 <- BS.readFile cabfn

      unless (cab0 == cab1) $ do
        print pi
        BS.writeFile cabfn cab0

        createDirectoryIfMissing True cabfnd2
        BS.writeFile cabfn2 cab0


    forceRm "./tmp"

    callProcess "hackage-repo-tool" [ "-v", "--expire-root", "99"
                                    , "update"
                                    , "--keys", "sdist/keys"
                                    , "--repo", "sdist/repo"
                                    ]

    removeDirectoryRecursive "./sdist/repo/index"
    removeFile "./sdist/repo/01-index.tar"

    return ()
  where


    forceRm fn = do
      whenM (doesDirectoryExist fn) $
        removeDirectoryRecursive fn

      -- verify post-condition
      whenM (doesDirectoryExist fn) $
        fail "failed to remove folder"




whenM :: Monad m => m Bool -> m () -> m ()
whenM p act = p >>= flip when act

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p act = p >>= flip unless act
