import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig

-- the main function
main :: IO ()
main = do
  wsbar <- spawnPipe myWsBar
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , borderWidth = 2
    , normalBorderColor = "#0033dd"
    , focusedBorderColor = "#99ccff"
    , terminal = "urxvt"
    , layoutHook = toggleLayouts (noBorders Full) $ myLayout
    , manageHook = myManageHook <+> manageDocks
    , logHook = myLogHook wsbar
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    }
    `additionalKeys`
    [
      ((mod1Mask, xK_space  ), spawn "exe=`dmenu_run -l 10 -fn 'Ricty-10' -nb '#1a1e1b' -nf '#d3d7cf'` && exec $exe")
    , ((0       , 0x1008FF11), spawn "amixer set Master 1.5dB-")
    , ((0       , 0x1008FF12), spawn "amixer set Master toggle")
    , ((0       , 0x1008FF13), spawn "amixer set Master 1.5dB+")
    ]
    `additionalKeysP`
    [
      ("M-m", sendMessage ToggleLayout)
    , ("M-e", spawn "nautilus")
    ]
    `removeKeysP`
    [
      -- unused dmenu shortcut
      "M-p"
    ]

modm = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5"]

gapwidth  = 8
gapwidthU = 0
gapwidthD = 0
gapwidthL = 10
gapwidthR = 10
tall = ResizableTall 1 (3/100) (1/2) []
myLayout = avoidStruts $ spacing gapwidth $
           gaps [(U, gapwidthU), (D, gapwidthD), (L, gapwidthL), (R, gapwidthR)] $
                 (ResizableTall 1 (3/100) (3/5) [])
             ||| (TwoPane (1/55) (1/2))
             ||| Simplest

-- myLayout = (spacing 8 $ ResizableTall 1 (3/100) (3/5) [])
--            ||| (spacing 2 $ (dragPane Horizontal (1/10) (1/2)))
--            ||| (dragPane Vertical (1/10) (1/2))

colorBlue      = "#857da9"
colorGreen     = "#88b986"
colorGray      = "#676767"
colorWhite     = "#d3d7cf"
colorGrayAlt   = "#313131"
colorNormalbg  = "#1a1e1b"
myStartupHook  = do
        spawnOnce "ibus-daemon -drx"
        spawnOnce "compton -cCGf -D 1 &"
        spawnOnce "sh $HOME/.fehbg"
myManageHook   = composeAll
    [ className =? "Mikutter.rb" --> doCenterFloat
    , className =? "Nautilus" --> doCenterFloat
    , className =? "Conky" --> doCenterFloat
    ]
myWsBar = "xmobar"
myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }
wsPP = xmobarPP { ppOrder = \(ws:l:t:_) -> [ws, t]
                , ppCurrent = xmobarColor colorGreen colorNormalbg . \s -> "●"
                , ppUrgent  = xmobarColor colorWhite colorNormalbg . \s -> "●"
                , ppVisible = xmobarColor colorWhite colorNormalbg . \s -> "●"
                , ppHidden  = xmobarColor colorWhite colorNormalbg . \s -> "●"
                , ppHiddenNoWindows = xmobarColor colorWhite colorNormalbg  . \s -> "○"
                , ppTitle   = xmobarColor colorGreen colorNormalbg
                , ppWsSep   = ""
                , ppSep     = "  "
                }
