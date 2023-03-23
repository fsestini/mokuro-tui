{-# LANGUAGE LambdaCase #-}

module Mokuro.TUI.Options where

import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(ReqArg),
      ArgOrder(Permute),
      OptDescr(..) )
import Control.Applicative (asum)
import Data.List.Extra (firstJust)
import Data.Maybe (listToMaybe)

newtype LibraryPath = LibraryPath { libPath :: FilePath } deriving Show

newtype Flag
    = UseMokuro String
      deriving Show

type ParsedArgs = ([Flag], [String])
newtype MokuroCommand = MokuroCommand { mokuroCommand :: String }

options :: [OptDescr Flag]
options =
  [ Option ['M'] ["use-mokuro"]  (ReqArg UseMokuro "COMMAND") "Custom mokuro command"
  ]

parseArgs :: [String] -> IO ParsedArgs
parseArgs argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: mlm [OPTION...]"

getMokuroOption :: ParsedArgs -> Maybe MokuroCommand
getMokuroOption = firstJust finder . fst
  where
    finder = \case { UseMokuro s -> Just (MokuroCommand s) ; _ -> Nothing }

getLibPathArg :: ParsedArgs -> Maybe LibraryPath
getLibPathArg = fmap LibraryPath . listToMaybe . snd
