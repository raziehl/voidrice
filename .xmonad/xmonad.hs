--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import System.IO (Handle, hPutStrLn)
import System.Exit
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO


import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize
import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import XMonad.Hooks.InsertPosition
import XMonad.Layout.Maximize
import XMonad.Actions.Minimize
import XMonad.Util.NamedScratchpad

myTerminal            = "urxvt"
myBorderWidth         = 0
myNormalBorderColor   = "#dddddd"
myFocusedBorderColor  = "#ff0000"
myModMask             = mod4Mask

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = manageDocks <+> myManageHook <+> manageHook desktopConfig,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<<"
          doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9","10"]
    where
               clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] l, let n = i ]


-- SCRATCHPADS

myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm
                ]

    where
    spawnTerm = myTerminal ++ "-n scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                where
                h = 0.9
                w = 0.9
                t = 0.95 -h
                l = 0.95 -w

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), spawn "rofi -show combi")
    , ((modm,               xK_q     ), spawn "kill -s USR1 $(pidof deadd-notification-center)")
    , ((modm .|. shiftMask, xK_q     ), kill)
    , ((modm,               xK_t ), sendMessage NextLayout)
    , ((modm, xK_b     ), sendMessage ToggleStruts)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_t ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    , ((modm, xK_Tab), nextWS)
    , ((modm .|. shiftMask, xK_Tab), prevWS)
    , ((modm .|. controlMask,               xK_Right), nextWS)
    , ((modm .|. controlMask,               xK_Left),  prevWS)
    , ((modm .|. controlMask .|. shiftMask, xK_Right), shiftToNext)
    , ((modm .|. controlMask .|. shiftMask, xK_Left),  shiftToPrev)
    , ((modm,                               xK_w     ),     toggleWS)
    , ((modm,                               xK_j     ), windows W.focusDown)
    , ((modm,                               xK_k     ), windows W.focusUp  )
    , ((modm,                               xK_Up     ), windows W.focusUp  )
    , ((modm,                               xK_Down     ), windows W.focusDown)
    , ((mod1Mask,                           xK_Tab     ), windows W.focusDown  )
    , ((mod1Mask .|. shiftMask,             xK_Tab     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm, xK_space), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm, xK_s     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_s     ), windows W.swapUp    )
    , ((modm,               xK_a     ), sendMessage Expand)
    , ((modm .|. shiftMask,               xK_a     ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_space     ), withFocused $ windows . W.sink)
    , ((modm,               xK_minus     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_minus     ), withLastMinimized maximizeWindowAndFocus)

    
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_c     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm, xK_f), sendMessage $ Toggle NBFULL)


    , ((0, xK_Print), spawn $ "scrot '%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f ~/Pictures'")
    -- , ((modm .|. shiftMask, xK_Print), spawn $ "scrot '%Y-%m-%d-%s_screenshot_$wx$h.jpg' -s -e 'mv $f ~/Pictures'")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
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
myLayout = avoidStruts
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               $ smartBorders
               $ tiled ||| ThreeColMid 1 (3/100) (1/2)  ||| ThreeCol 1 (3/100) (1/2) ||| Grid ||| spiral (6/7) ||| noBorders Full
                    where
                    tiled   = Tall nmaster delta ratio
                    nmaster = 1
                    delta   = 3/100
                    ratio   = 1/2


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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doCenterFloat
    , className =? "Gimp"           --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Gimp"           --> doCenterFloat
    , className =? "rdesktop"       --> doCenterFloat
    , resource  =? "Dialog"         --> doCenterFloat
    ]
    --  <+> insertPosition Below Newer

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--



main = do
  xmproc <- spawnPipe "xmobar -x 0"
  xmonad $ docks defaults {
    logHook = dynamicLogWithPP $ def {
      ppOutput = \x -> System.IO.hPutStrLn xmproc x,
      ppCurrent = xmobarColor myNormalBorderColor "" . wrap """",
      ppVisible = xmobarColor myFocusedBorderColor "" . wrap """",
      ppHidden = wrap """",
      ppHiddenNoWindows = xmobarColor "#262626" "",
      ppUrgent = xmobarColor myFocusedBorderColor "",
      ppSep = " ",
      ppWsSep = " ",
      ppLayout = (\ x -> case x of
        "Spacing Tall"                 -> "<fn=1>Tall</fn>"
        "Spacing Grid"                 -> "<fn=1>Grid</fn>"
        "Spacing Spiral"               -> "<fn=1>spiral</fn>"
        "Spacing ThreeCol"             -> "<fn=1>ThreeColMid</fn>"
        "Spacing Full"                 -> "<fn=1>Full</fn>"
        _                                         -> x)
      
    }
  }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
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
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
