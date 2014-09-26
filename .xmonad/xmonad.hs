{-# LANGUAGE DeriveDataTypeable #-}

import Data.Ratio
import Data.Maybe
import Data.Monoid

import XMonad
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.OnScreen
import XMonad.Actions.SwapWorkspaces
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.XUtils
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.Hooks.DynamicLog
import System.IO

import qualified Data.Map as Map
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- workspaces: named from 0 to 9
myWorkspaces = map show [0 .. 9]

layout = id
        . smartBorders
        . mkToggle (NOBORDERS ?? FULL ?? EOT)
        $ smartSpacing 5 $ tiled ||| Mirror tiled ||| Full
          where tiled = Tall 1 (3/100) (1/2)

-- map from WorkspaceId to ScreenId with the initial position of the workspaces
myWorkspacesInitialPosition = Map.fromList $ zip myWorkspaces [0, 0, 0, 0, 0, 1, 1, 1, 1, 1]

-- WorkspaceScreen stores a mapping from WorkspaceId to ScreenId.
-- For more info on how this is used see:
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-ExtensibleState.html
data WorkspaceScreen = WorkspaceScreen (Map.Map WorkspaceId ScreenId) deriving Typeable
instance ExtensionClass WorkspaceScreen where
  initialValue = WorkspaceScreen myWorkspacesInitialPosition

-- Updates the screen assigned to a workspace
doUpdateWorkspaceScreen :: WorkspaceId -> ScreenId -> X ()
doUpdateWorkspaceScreen wid sc = do
    (WorkspaceScreen m) <- XS.get
    XS.put $ WorkspaceScreen $ Map.insert wid sc m

-- Updates the screen assigned to the currently focused workspace
doUpdateCurrentWorkspaceScreen :: ScreenId -> X ()
doUpdateCurrentWorkspaceScreen sc = do
  wid <- withWindowSet $ return . W.currentTag
  doUpdateWorkspaceScreen wid sc

-- Gets the screen assigned to a workspace
getWorkspaceScreen :: WorkspaceId -> WorkspaceScreen -> ScreenId
getWorkspaceScreen wid (WorkspaceScreen m) = fromJust $ Map.lookup wid m

-- Shows a workspace in the screen assigned to it
doViewWorkspace :: WorkspaceId -> X ()
doViewWorkspace wid = do
  sc <- XS.gets $ getWorkspaceScreen wid
  windows $ moveWorkspaceToScreen wid sc

-- Shows the current workspace in the screen assigned to it
doViewCurrentWorkspace :: X ()
doViewCurrentWorkspace = do
  wid <- withWindowSet $ return . W.currentTag
  doViewWorkspace wid

-- moves a workspace to a given screen and sets focus to that workspace
moveWorkspaceToScreen :: WorkspaceId -> ScreenId -> WindowSet -> WindowSet
moveWorkspaceToScreen wid sc ws =
  W.view wid $ greedyViewOnScreen sc wid ws

-- Forward the window information to the left dzen bar and format it
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -x '0' -w '1920' -ta 'l'" ++ myDzenStyle
-- myDzenConky  = "conky -c ~/.conkyrc | dzen2 -x '1000' -w '920' -ta 'r'" ++ myDzenStyle

-- Bar style 24px high and colors
myDzenStyle  = " -h '20' -y '0' -fg '#777777' -bg '#222222'"

-- Very plain formatting, non-empty workspaces are highlighted,
-- urgent workspaces (e.g. active IM window) are highlighted in red
myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = "  "
    , ppLayout  = \y -> ""
    , ppTitle   = dzenColor "#ffffff" "" . wrap " " " "
    }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

-- The main function.
main = do
  leftbar <- statusBar myDzenStatus myDzenPP toggleStrutsKey myconfig
  -- rightbar <- statusBar myDzenConky myDzenPP toggleStrutsKey leftbar

  xmonad leftbar

myconfig = defaultConfig {
    modMask = mod4Mask,
    borderWidth = 0,
    layoutHook = layout
  }

  `additionalKeys`

  ([
    -- mod4 + shift + {w,e} -> move workspace to screen 1, 2 and update assigned screen
      ((mod4Mask .|. shiftMask, k), (doUpdateCurrentWorkspaceScreen i) >> doViewCurrentWorkspace)
        | (k, i) <- zip [xK_w, xK_e] [0..]
  ] ++ [
      ((mod4Mask .|. shiftMask, xK_h), windows W.swapMaster)
  ] ++ [
    ((mod4Mask, xK_f), sendMessage $ Toggle FULL)
  ])

  `removeKeys`

  [
      (mod4Mask, xK_r)
    , (mod4Mask, xK_p)
    , (mod4Mask, xK_Return)
  ]
