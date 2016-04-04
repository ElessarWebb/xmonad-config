{-# LANGUAGE DeriveDataTypeable #-}


import Data.Ratio
import Data.Maybe
import Data.Monoid
import Data.Map as Map

import XMonad
import XMonad.Config.Xfce
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
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Magnifier hiding (Toggle)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import System.IO

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- workspaces: named from 0 to 10
myWorkspaces = [ "SH", "Web", "Code", "PDF", "Chat", "VI", "VII", "VIII", "Music" ]


-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabconfig = defaultTheme {
  activeBorderColor = "#002b36",
  activeTextColor = "#ffffff",
  activeColor = "#268bd2",
  inactiveBorderColor = "#002b36",
  inactiveTextColor = "#eeeeee",
  inactiveColor = "#002b36"
}

layout = gaps [(U, 35), (R, 0)]
        . smartBorders
        . smartSpacing 5
        . mkToggle (NOBORDERS ?? FULL ?? EOT)
        $ tiled ||| Grid ||| tabbed shrinkText tabconfig ||| Full
          where tiled = ResizableTall 1 (3/100) (2/3) []

-- Forward the window information to the left dzen bar and format it
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -x '0' -w '750' -ta 'l'" ++ " -h '25' -y '1055' -fg '#eeeeee' -bg '#002b36'"

-- Very plain formatting, non-empty workspaces are highlighted,
-- urgent workspaces (e.g. active IM window) are highlighted in red
myDzenPP  = dzenPP
    { ppCurrent = wrap "  " "  " . (c "#8FC0CC")
    , ppHidden  = c "#1E6E82" . wrap "  " "  "
    , ppHiddenNoWindows = \y -> ""
    , ppUrgent  = c "#8FC0CC" . wrap "  " "  "
    , ppSep     = "   |   "
    , ppLayout  = const ""
    , ppTitle   = const ""
    }
    where c = flip dzenColor ""

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

main = do

  -- spawn conky
  -- conky <- spawnPipe "conky -c ~/.xmonad/conkyrc"

  -- spawn dzen
  -- leftbar <- statusBar myDzenStatus myDzenPP 

  -- hopla
  xmonad $ ewmh myconfig

myconfig = defaultConfig {
    modMask = mod4Mask,
    borderWidth = 1,
    normalBorderColor = "#002b36",
    focusedBorderColor = "#2A788C",
    layoutHook = avoidStruts layout,
    workspaces = myWorkspaces,
    handleEventHook = fullscreenEventHook <+> docksEventHook,
    manageHook = fullscreenManageHook <+> (isFullscreen --> doFullFloat) <+> manageDocks,
    startupHook = setWMName "LG3D"
  }

  `additionalKeys`

  ([
    -- m-s-h to swap to master
      ((mod4Mask .|. shiftMask, xK_h), windows W.swapMaster)

    -- m-f full screen toggles strut
    , ((mod4Mask, xK_f), (sendMessage $ Toggle FULL) >>= (\_ -> sendMessage $ ToggleStrut U))
    -- m-b toggles strut
    , ((mod4Mask, xK_b), (sendMessage $ ToggleStrut U))

    -- c-m to toggle magnification
    , ((mod4Mask .|. controlMask, xK_m), (sendMessage Mag.Toggle ))
    , ((mod4Mask,               xK_z), sendMessage MirrorShrink)
    , ((mod4Mask,               xK_a), sendMessage MirrorExpand)
    -- utility
    , ((mod4Mask .|. shiftMask, xK_r), spawn "xmonad --recompile")
    , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
    , ((mod4Mask .|. shiftMask, xK_g), gotoMenu)
    , ((mod4Mask .|. shiftMask, xK_b), bringMenu)
  ])

  `removeKeys`

  [
      (mod4Mask, xK_r)
    , (mod4Mask, xK_p)
    , (mod4Mask, xK_Return)
  ]
