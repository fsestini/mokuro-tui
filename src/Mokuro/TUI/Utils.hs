module Mokuro.TUI.Utils where

import qualified Data.Set as Set
import Shelly (runFoldLines, Sh, catchany_sh, run_, shelly)
import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (bool)

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' x y z = maybe y z x

findAll :: FilePath -> Sh [Text]
findAll fp = runFoldLines [] (flip (:)) "find" [pack fp]

runWhich :: MonadIO m => Text -> m Bool
runWhich x = liftIO . shelly $
  catchany_sh (run_ "which" [x] >> pure True) (const (pure False))

wrapFilePath :: FilePath -> FilePath
wrapFilePath x = "\"" ++ x ++ "\""

ioUserErr :: MonadIO m => String -> m ()
ioUserErr = liftIO . ioError . userError

ensureTrue :: MonadIO m => m Bool -> String -> m ()
ensureTrue m s = m >>= bool (ioUserErr s) (pure ())
