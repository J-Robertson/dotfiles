{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Arrow (second)
import XMonad
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Spacing
import System.Exit
import Data.Monoid (All)
import XMonad.Layout.LayoutModifier
import XMonad.Util.Font (fi)
import XMonad.Hooks.DynamicBars as Bars
import XMonad.Util.Run

myTerminal :: String
myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 1

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor :: String
myFocusedBorderColor :: String
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#ffffff"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf $
  [("M-b", sendMessage ToggleStruts)
  ,("M-S-<Return>", spawn $ terminal conf)
  ,("M-p", spawn "dmenu_run")
  ,("M-S-c", kill)
  ,("M-<Space>", sendMessage NextLayout)
  ,("M-S-<Space>", setLayout $ layoutHook conf)
  ,("M-n", refresh)
  ,("M-<Tab>", onGroup W.focusDown')
  -- ,("M-j", windowGo D True)
  -- ,("M-k", windowGo U True)
  -- ,("M-h", windowGo L True)
  -- ,("M-l", windowGo R True)
  ,("M-j", condX (sendMessage $ Go D) (windows W.focusDown))
  ,("M-k", condX (sendMessage $ Go U) (windows W.focusUp))
  ,("M-h", condX (sendMessage $ Go L) (windows W.focusDown))
  ,("M-l", condX (sendMessage $ Go R) (windows W.focusUp))
  ,("M-<Return>", windows W.swapMaster)
  -- ,("M-S-j", windowSwap D True)
  -- ,("M-S-k", windowSwap U True)
  -- ,("M-S-h", windowSwap L True)
  -- ,("M-S-l", windowSwap R True)
  ,("M-S-j", sendMessage $ Swap D)
  ,("M-S-k", sendMessage $ Swap U)
  ,("M-S-h", sendMessage $ Swap L)
  ,("M-S-l", sendMessage $ Swap R)
  ,("M-C-j", sendMessage $ pullGroup D)
  ,("M-C-k", sendMessage $ pullGroup U)
  ,("M-C-h", sendMessage $ pullGroup L)
  ,("M-C-l", sendMessage $ pullGroup R)
  ,("M-C-u", withFocused (sendMessage . UnMerge))
  ,("M-=", incSpacing 1)
  ,("M--", incSpacing (-1))
  ,("M-0", setSpacing 0)
  ,("M-]", sendMessage Expand)
  ,("M-[", sendMessage Shrink)
  ,("M-t", withFocused $ windows . W.sink)
  ,("M-,", sendMessage (IncMasterN 1))
  ,("M-.", sendMessage (IncMasterN (-1)))
  ,("M-S-q", spawn "systemctl poweroff")
  ,("M-q", spawn "rm .xmonad/xmonad.state; xmonad --recompile; xmonad --restart")
  ,("M-r", spawn "systemctl reboot")
  ,("M-S-z", spawn "sleep 0.1; xset dpms force off; slock; xset -dpms")
  ,("M-S-r", spawn "systemctl hibernate; slock")
  ,("M-x", io exitSuccess)
  ,("M-e", spawn "emacsclient -c")
  ,("M-w", spawn "qutebrowser")
  ,("<XF86AudioMute>", spawn "pamixer -t")
  ,("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
  ,("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  ,("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  ,("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  ,("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle")
  ,("M-m", withFocused f)]
  ++
  [("M" ++ mask ++ tag, windows $ f tag)
  | tag <- myWorkspaces, (f,mask) <- [(W.greedyView, "-"), (W.shift, "-S-")]]
  ++
  [("M" ++ mask ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
  | (key,sc) <- zip ["s","d","f"] [0..], (f,mask) <- [(W.view, "-"), (W.shift, "-S-")]]

condX :: X () -> X () -> X ()
condX f g = do
  l  <- (description . W.layout . W.workspace . W.current) <$> gets windowset
  if l/="Full"
    then f
    else g


f :: Window -> X ()
f w = do
  cn <- runQuery className w
  spawn $ "pactl set-sink-input-mute $(pactl list sink-inputs | grep --before-context=25 -i \"" ++
    (case cn of
       "qutebrowser" -> "chromium-browser"
       _ -> cn) ++
    "\" | grep \"Sink Input\" | awk -F '#' '{print $2}') toggle"
  spawn $ "echo \""++cn++"\" > test.out"
  -- title <- runQuery title w
  -- spawn $ "pactl set-sink-input-mute $(pactl list sink-inputs | grep --before-context=25 -i \"" ++ title ++ "\" | grep \"Sink Input\" | awk -F '#' '{print $2}') toggle"


myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    ,((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)]


myTabTheme :: Theme
myTabTheme = def
  { activeColor         = "#268bd2"
  , inactiveColor       = "#073642"
  , activeBorderColor   = "#268bd2"
  , inactiveBorderColor = "#073642"
  , fontName            = "xft: DejaVu Sans Mono-8"
  , decoHeight          = 13
  }

myLayout = windowNavigation $ lessBorders Screen mainLayout ||| noBorders Full
  where
    mainLayout = named "Tall"
                 $ addTabs shrinkText myTabTheme
                 $ subLayout [] Simplest
                 $ Tall nmaster delta ratio


    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    named n = renamed [XMonad.Layout.Renamed.Replace n]

myManageHook :: ManageHook
myManageHook = mconcat
    [ className =? "Gimp"           --> doFloat
    , isDialog                      --> doFloat
    , isFullscreen                  --> doFloat]

myEventHook :: Event -> X All
myEventHook = Bars.dynStatusBarEventHook xmobarCreator xmobarDestroyer

myLogHook :: X ()
myLogHook = Bars.multiPP myPP (myPP {ppCurrent = xmobarColor "#ec7373" ""})

myPP :: PP
myPP = def
      {
        ppCurrent = xmobarColor "#60dc80" ""
      , ppTitle   = xmobarColor "#ec7373" "" . shorten 80
      , ppSep     = " | "
      }

myStartupHook :: X ()
myStartupHook = do
  spawn "compton --no-fading-openclose"
  spawn "wmname XMonad"
  spawn "xrandr --auto"
  spawn "~/dotfiles/monitorscript.sh"
  spawn "xset -dpms; xset s off"
  spawn "xrdb ~/.Xresources"
  spawn "feh --bg-max ~/Downloads/buddhabrot6.png"
  spawn "xset r rate 300 40"
  spawn "pgrep emacs || emacs --daemon"
  setDefaultCursor xC_left_ptr
  Bars.dynStatusBarStartup xmobarCreator xmobarDestroyer

xmobarCreator :: Bars.DynamicStatusBar
xmobarCreator (S sid) = spawnPipe $ "xmobar -x " ++ show sid

xmobarDestroyer :: Bars.DynamicStatusBarCleanup
xmobarDestroyer = return ()

main :: IO ()
main = xmonad . docks $ myConfig

myConfig = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = avoidStruts myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
