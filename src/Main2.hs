module Main where

import           Cabal.Plan
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256     as SHA256
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

    -- putStrLn "pre-fetching packages..."
    -- callProcess "cabal" (["fetch", "--no-dependencies"] ++ pids')

    createDirectoryIfMissing True "./dependencies"

    forM_ (Map.toList pids) $ \(pi@(PkgId (PkgName pnt) pv),sh) -> do
      let tarfn = T.unpack (dispPkgId pi) <.> "tar.gz"
          pn = T.unpack pnt
          cacheTarFn = cabalPkgCacheDir </> pn </> (T.unpack (dispVer pv)) </> tarfn
          localTarFn = "./dependencies" </> tarfn

      -- print (localTarFn, cacheTarFn, sh)

      unlessM (doesFileExist localTarFn) $ do

        unlessM (doesFileExist cacheTarFn) $ do
          callProcess "cabal" ["fetch", "--no-dependencies", T.unpack (dispPkgId pi)]

        -- TODO: verify sha256
        BS.writeFile localTarFn =<< BS.readFile cacheTarFn

      Just sh' <- (sha256FromByteString . SHA256.hash) <$> BS.readFile localTarFn

      unless (sh' == sh) $ do
        fail ("SHA256 mismatch for " ++ show localTarFn)

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
