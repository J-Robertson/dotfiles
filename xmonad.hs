--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Hooks.DynamicLog
import System.Exit
import XMonad.Layout.ResizableTile

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Cursor
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

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal      = "xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myFocusedBorderColor :: String
myNormalBorderColor  = "#555555"
myFocusedBorderColor = "#dddddd"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
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
  ,("M-e", spawn "emacs")
  ,("M-y", fun)]

  ++

  [("M" ++ mask ++ tag, windows $ f tag)
  | tag <- myWorkspaces, (f,mask) <- [(W.greedyView, "-"), (W.shift, "-S-")]]





fun :: X ()
fun = do
  XState {windowset = old} <- get
  spawn ("echo \"" ++ (show (W.stack (W.workspace (W.current old)))) ++ "\" > test.out")


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

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
                 $ ResizableTall nmaster delta ratio []

    noBar      = named "noBar Tabbed/Tall"
                 $ windowNavigation
                 $ addTabs shrinkText myTabTheme
                 $ subLayout [] (Simplest ||| Accordion)
                 $ spacing 4
                 $ ResizableTall nmaster delta ratio []

    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    named n = renamed [XMonad.Layout.Renamed.Replace n]


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = mconcat
    [ className =? "Gimp"           --> doFloat
    , resource  =? "vlc"            --> doFloat
    , isDialog                      --> doFloat
    , isFullscreen                  --> doFloat]


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
  spawn "xset -dpms; xset s off"
  spawn "feh --bg-scale /usr/share/backgrounds/gnome/Dark_Ivy.jpg"
  setDefaultCursor xC_left_ptr

------------------------------------------------------------------------
-- Run xmonad with xmobar and with a command to stop screen turn off
myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def { unmappedWindowRect = [("Full", singleWindowRect)]
                           , defaultTiledNavigation = centerNavigation}

main :: IO ()
main = statusBar myBar myPP toggleStrutsKey myConfig >>= xmonad . withNavigation2DConfig myNavigation2DConfig

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
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
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

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The modifier key is 'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch gnome-terminal",
    "mod-p            Launch dmenu",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "mod-i  Push the borders up",
    "mod-o  Push the borders down",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging",
    "",
    "-- Xmobar",
    "mod-b  Toggle appearence of statusbar",
    "",
    "-- lockscreen",
    "mod-Shift-z  do lockscreen",
    "",
    "-- Start emacs",
    "mod-e Start emacs"]
