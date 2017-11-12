import XMonad
import XMonad.Hooks.DynamicLog
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Accordion
import XMonad.Actions.Navigation2D
import XMonad.Layout.Spacing
import Data.Monoid (All)

myTerminal :: String
myTerminal      = "xterm"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 0

myModMask :: KeyMask
myModMask       = mod4Mask

myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor :: String
myFocusedBorderColor :: String
myNormalBorderColor  = "#555555"
myFocusedBorderColor = "#dddddd"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = \conf -> mkKeymap conf $
  [("M-S-<Return>", spawn $ terminal conf)
  ,("M-p", spawn "dmenu_run")
  ,("M-S-c", kill)
  ,("M-<Space>", sendMessage NextLayout)
  ,("M-S-<Space>", setLayout $ layoutHook conf)
  ,("M-n", refresh)
  ,("M-m", windows W.focusMaster)
  ,("M-<Tab>", onGroup W.focusDown')
  ,("M-j", windowGo D True)
  ,("M-k", windowGo U True)
  ,("M-h", windowGo L True)
  ,("M-l", windowGo R True)
  ,("M-<Return>", windows W.swapMaster)
  ,("M-S-j", windowSwap D True)
  ,("M-S-k", windowSwap U True)
  ,("M-S-h", windowSwap L True)
  ,("M-S-l", windowSwap R True)
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
  ,("M-S-q", io exitSuccess)
  ,("M-q", spawn "xmonad --recompile; xmonad --restart")
  ,("M-S-z", spawn "slock")
  ,("M-e", spawn "emacs")]
  ++
  [("M" ++ mask ++ tag, windows $ f tag)
  | tag <- myWorkspaces, (f,mask) <- [(W.greedyView, "-"), (W.shift, "-S-")]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    ,((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)]

myTopBarTheme :: Theme
myTopBarTheme = def
  { activeColor         = "#268bd2"
  , inactiveColor       = "#073642"
  , activeBorderColor   = "#268bd2"
  , inactiveBorderColor = "#073642"
  , activeTextColor     = "#268bd2"
  , inactiveTextColor   = "#073642"
  , decoHeight          = 10
  }

myTabTheme :: Theme
myTabTheme = def
  { activeColor         = "#268bd2"
  , inactiveColor       = "#073642"
  , activeBorderColor   = "#268bd2"
  , inactiveBorderColor = "#073642"
  , fontName            = "xft: DejaVu Sans Mono-8"
  , decoHeight          = 15
  }

myLayout = mainLayout ||| noBar ||| Full
  where
    mainLayout = named "Tabbed/Tall"
                 $ windowNavigation
                 $ noFrillsDeco shrinkText myTopBarTheme
                 $ addTabs shrinkText myTabTheme
                 $ subLayout [] (Simplest ||| Accordion)
                 $ spacing 4
                 $ Tall nmaster delta ratio

    noBar      = named "noBar Tabbed/Tall"
                 $ windowNavigation
                 $ addTabs shrinkText myTabTheme
                 $ subLayout [] (Simplest ||| Accordion)
                 $ spacing 4
                 $ Tall nmaster delta ratio

    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    named n = renamed [XMonad.Layout.Renamed.Replace n]

myManageHook :: ManageHook
myManageHook = mconcat
    [ className =? "Gimp"           --> doFloat
    , resource  =? "vlc"            --> doFloat
    , isDialog                      --> doFloat
    , isFullscreen                  --> doFloat]

myEventHook :: Event -> X All
myEventHook = mempty

myLogHook :: X ()
myLogHook = return ()

myStartupHook :: X ()
myStartupHook = do
  spawn "xset -dpms; xset s off"
  spawn "feh --bg-scale /usr/share/backgrounds/gnome/Dark_Ivy.jpg"
  spawn "xset r rate 300 40"
  setDefaultCursor xC_left_ptr

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def { unmappedWindowRect = [("Full", singleWindowRect)]
                           , defaultTiledNavigation = centerNavigation}

main :: IO ()
main = statusBar myBar myPP toggleStrutsKey myConfig
       >>= xmonad . withNavigation2DConfig myNavigation2DConfig

myBar :: String
myBar = "xmobar"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modm} = (modm, xK_b)

myPP :: PP
myPP = def
      {
        ppCurrent = xmobarColor "#60dc80" ""
      , ppTitle   = xmobarColor "#ec7373" "" . shorten 80
      , ppSep     = " | "
      }

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
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
