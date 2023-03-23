-- | Types

module Mokuro.TUI.Types where

import Mokuro.TUI.Options (MokuroCommand, LibraryPath(..))

import qualified Brick.Widgets.List as L (List, list)

import qualified Data.Vector as Vec (fromList)
import Data.List (isSuffixOf)
import Data.List.Extra (replace)
import Lens.Micro (Lens', lens)

data ListEntry = ListEntry
  { entryFilePath :: FilePath
  , entrySelected :: Bool
  , prettyDesc :: String
  } deriving Show

mkListEntry :: LibraryPath -> FilePath -> ListEntry
mkListEntry (LibraryPath lib) fp = ListEntry fp False pretty
  where
    len = length lib + if "/" `isSuffixOf` lib then 0 else 1
    pretty = replace "/" " / " . drop len $ fp

type DirectoryList = L.List () ListEntry

newtype AppState = AppState { stateList :: DirectoryList }

mkState :: LibraryPath -> [FilePath] -> AppState
mkState lib fps =
  AppState (L.list () (Vec.fromList . map (mkListEntry lib) $ fps) 1)

stateListL :: Lens' AppState DirectoryList
stateListL = lens stateList (\s l -> s { stateList = l })

entrySelectedL :: Lens' ListEntry Bool
entrySelectedL = lens entrySelected (\e b -> e { entrySelected = b })
