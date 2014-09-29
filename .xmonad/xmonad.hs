{-# LANGUAGE DeriveDataTypeable #-}

import Data.Ratio
import Data.Maybe
import Data.Monoid
import Data.Map as Map

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
import XMonad.Layout.Fullscreen
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import System.IO

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- workspaces: named from 0 to 9
myWorkspaces = [ "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X" ]

layout = id
        . smartBorders
        . smartSpacing 5
        . mkToggle (NOBORDERS ?? FULL ?? EOT)
        $ tiled ||| Mirror tiled ||| Full
          where tiled = Tall 1 (3/100) (1/2)

-- Forward the window information to the left dzen bar and format it
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -x '0' -w '1920' -ta 'l'" ++ " -h '20' -y '0' -fg '#777777' -bg '#222222'"

-- Very plain formatting, non-empty workspaces are highlighted,
-- urgent workspaces (e.g. active IM window) are highlighted in red
myDzenPP  = dzenPP
    { ppCurrent = wrap (c "#ffffff" " [ ") (c "#ffffff" " ] ") . (c "#268bd2")
    , ppHidden  = c "#dddddd" . wrap " " " "
    , ppHiddenNoWindows = \y -> ""
    , ppUrgent  = c "#ff0000" . wrap " " " "
    , ppSep     = "   |   "
    , ppLayout  = \y -> ""
    , ppTitle   = wrap (c "#ffffff" " [ ") (c "#ffffff" " ] ") . (c "#859900")
    }
    where c = flip dzenColor ""

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

main = do

  -- spawn conky
  conky <- spawnPipe "conky -c ~/.xmonad/conkyrc"

  -- spawn dzen
  leftbar <- statusBar myDzenStatus myDzenPP toggleStrutsKey myconfig

  -- hopla
  xmonad leftbar

myconfig = defaultConfig {
    modMask = mod4Mask,
    borderWidth = 0,
    layoutHook = layout,
    workspaces = myWorkspaces,
    handleEventHook = fullscreenEventHook <+> docksEventHook,
    manageHook = fullscreenManageHook <+> manageDocks,
    startupHook = setWMName "LG3D"
  }

  `additionalKeys`

  ([
    -- client management
      ((mod4Mask .|. shiftMask, xK_h), windows W.swapMaster)
    , ((mod4Mask, xK_f), (sendMessage $ Toggle FULL) >>= (\x -> sendMessage $ ToggleStrut U))

    -- utility
    , ((mod4Mask .|. shiftMask, xK_r), spawn "xmonad --recompile")
  ])

  `removeKeys`

  [
      (mod4Mask, xK_r)
    , (mod4Mask, xK_p)
    , (mod4Mask, xK_Return)
  ]
