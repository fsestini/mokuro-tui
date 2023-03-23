{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Mokuro.TUI (tui) where

import Mokuro.TUI.Utils
    ( nubOrd, maybe', findAll, runWhich, wrapFilePath, ioUserErr, ensureTrue )
import Mokuro.TUI.Options
    ( getMokuroOption, parseArgs, MokuroCommand (..), getLibPathArg,
      LibraryPath(..) )
import Mokuro.TUI.Types
    ( AppState(..), ListEntry(..), mkState, stateListL, entrySelectedL )

import Lens.Micro((^.), traversed)
import Lens.Micro.Mtl(zoom)

import Shelly ( shelly, Sh, runFoldLines )
import Shelly.Lifted (fromText, silently, test_d)

import Data.Text (Text, pack)
import qualified Data.Text as T
import System.FilePath (takeDirectory)
import System.Environment (getArgs)

import qualified Graphics.Vty as V

import qualified Brick.Widgets.List as L
import Brick.Main (defaultMain, App (..), neverShowCursor, halt)
import Brick.Types (modify)
import qualified Brick.AttrMap as A
import Brick.Util (on, fg)
import Brick
    ( BrickEvent(VtyEvent), EventM, Widget, str, hLimit, vLimit, vBox, hBox,
      fill, put )

import qualified Data.Vector as Vec
import Data.Bool (bool)
import Data.Tuple.Extra ((&&&))
import System.Posix (forkProcess, executeFile, getProcessStatus)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (vCenter, hCenter)

isImagePath :: Text -> Bool
isImagePath t =
  T.isSuffixOf ".jpeg" t || T.isSuffixOf ".jpg" t || T.isSuffixOf ".png" t

findTargetDirs :: LibraryPath -> Sh [FilePath]
findTargetDirs = silently . fmap filterDirs . findAll . libPath
  where filterDirs = nubOrd . map (takeDirectory . fromText) . filter isImagePath

brickApp :: App AppState () ()
brickApp = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
  where
    l = stateList s
    label = str "Available locations"
    box = borderWithLabel label $
          hLimit 60 $
          vLimit 25 $
          L.renderList listDrawElement True l
    ui = vCenter . vBox . map hCenter . (box :) . map str $
      [ " "
      , "Press ENTER to toggle-queue entry for processing."
      , "Press 'p' to process all queued entries."
      , "Press ESC to exit."
      ]

listDrawElement :: Bool -> ListEntry -> Widget ()
listDrawElement _ e =
  str . (if entrySelected e then ("[x] " ++) else ("[ ] " ++)) $ prettyDesc e

appEvent :: BrickEvent () () -> EventM () AppState ()
appEvent (VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] ->
      zoom (stateListL . L.listSelectedElementL . entrySelectedL) (modify not)
    V.EvKey V.KEsc [] -> do
      zoom (stateListL . L.listElementsL . traversed . entrySelectedL)
        (put False)
      halt
    V.EvKey (V.KChar 'p') [] -> halt
    e -> zoom stateListL (L.handleListEvent e)
appEvent _ = pure ()

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    ]

runTUI :: MokuroCommand -> AppState -> IO ()
runTUI c st = do
  finalState <- defaultMain brickApp st
  let args = map (wrapFilePath . entryFilePath)
           . filter entrySelected . Vec.toList
           $ finalState^.stateListL . L.listElementsL
  if null args
    then putStrLn "Nothing to do."
    else executeFile (mokuroCommand c) True
           (args ++ ["--disable-confirmation"]) Nothing

tui :: IO ()
tui = do
  args <- getArgs
  parsed <- parseArgs args
  let (comm, errMsg) = maybe' (getMokuroOption parsed)
                         ("mokuro", commErr "mokuro")
                         ((id &&& commErr) . mokuroCommand)
  runWhich (pack comm) >>= bool (ioUserErr errMsg) (pure ())

  let mlib = getLibPathArg parsed
  maybe' mlib (ioUserErr "wrong number of arguments") $ \lib ->
    shelly (ensureTrue (test_d (libPath lib)) "directory does not exist"
            >> findTargetDirs lib) >>= runTUI (MokuroCommand comm) . mkState lib
  where
    commErr :: String -> String
    commErr c = mconcat
      [ "`" , c , "` command not found. "
      , "Please specify a working command with --use-mokuro COMMAND."
      ]
