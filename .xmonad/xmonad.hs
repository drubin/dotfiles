import XMonad                          -- (0) core xmonad libraries
 
import qualified XMonad.StackSet as W  -- (0a) window stack manipulation
import qualified Data.Map as M         -- (0b) map creation
 
-- Hooks -----------------------------------------------------
 
import XMonad.Hooks.DynamicLog     -- (1)  for dzen status bar
import XMonad.Hooks.UrgencyHook    -- (2)  alert me when people use my nick
                                   --      on IRC
import XMonad.Hooks.ManageDocks    -- (3)  automatically avoid covering my
                                   --      status bar with windows
import XMonad.Hooks.ManageHelpers  -- (4)  for doCenterFloat, put floating
                                   --      windows in the middle of the
                                   --      screen
 
-- Layout ----------------------------------------------------
 
import XMonad.Layout.ResizableTile -- (5)  resize non-master windows too
import XMonad.Layout.Grid          -- (6)  grid layout
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders     -- (7)  get rid of borders sometimes
                                   -- (8)  navigate between windows
import XMonad.Layout.WindowNavigation  --  directionally
import XMonad.Layout.Named         -- (9)  rename some layouts
import XMonad.Layout.PerWorkspace  -- (10) use different layouts on different WSs
import XMonad.Layout.WorkspaceDir  -- (11) set working directory
                                   --      per-workspace
import XMonad.Layout.Reflect       -- (13) ability to reflect layouts
import XMonad.Layout.MultiToggle   -- (14) apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances
                                   -- (15) ability to magnify the focused
                                   --      window
import XMonad.Layout.Simplest

import qualified XMonad.Layout.Magnifier as Mag
 
import XMonad.Layout.Gaps
 
-- Actions ---------------------------------------------------
 
import XMonad.Actions.CycleWS      -- (16) general workspace-switching
                                   --      goodness
import XMonad.Actions.CycleRecentWS
                                   -- (17) more flexible window resizing
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Warp         -- (18) warp the mouse pointer
import XMonad.Actions.Submap       -- (19) create keybinding submaps
import XMonad.Actions.Search       -- (20) some predefined web searches
import XMonad.Actions.WindowGo     -- (21) runOrRaise
import XMonad.Actions.UpdatePointer -- (22) auto-warp the pointer to the LR
                                    --      corner of the focused windo


import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "/usr/bin/xterm"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
--myModMask		= mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","FireFox","3","4","5","6","7","8","9"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
 
    [
 
    -- launch a terminal
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf )
    , ((modm .|. shiftMask, xK_KP_Enter), spawn $ XMonad.terminal conf )
    -- ctrl+alt+space
    , ((mod1Mask .|. controlMask, xK_space), spawn $ XMonad.terminal conf )
    -- ((modm ,xK_Return), spawn "gnome-terminal" )
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

	-- Take a screenshot
 
	, ((modm,				xK_s	 ),	spawn "scrot '/home/drubin/Screenshots/%Y-%m-%d-%H%M%S.png'" )
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_g), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    , ((modm , xK_b ), sendMessage ToggleStruts)

    -- toggle borders
    , ((modm .|. shiftMask, xK_b	),	sendMessage $ Toggle NOBORDERS)

	-- Moving workspaces
	, ((modm,				xK_Left	),	prevWS		)
	, ((modm,				xK_Right),	nextWS		)
	, ((modm .|. shiftMask,	xK_Left	),	shiftToPrev	)
	, ((modm .|. shiftMask,	xK_Right),	shiftToNext	)

	-- Eject cdrom
	, ((modm,				xK_Delete),	spawn "eject /dev/cdrom")

    -- Lock the screen
    , ((modm .|. shiftMask, xK_l	),	spawn "xscreensaver-command -lock")

	-- Suspend
	, ((modm .|. shiftMask,	xK_s	),	spawn "suspend.sh Suspend")

	-- Hibernate
	, ((modm .|. shiftMask,	xK_h	),	spawn "suspend.sh Hibernate")
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)
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
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
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
myLayout = onWorkspace "9" (noBorders Simplest) $
	tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 
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
    [ className =? "MPlayer"        	--> doFloat
    ,  className =? "tottem"		--> doFloat
    , className =? "VLC (XVideo output)" --> doFullFloat
    , className =? "Gimp"           	--> doFloat
	, className	=? "Mtpaint"			--> doFloat
	, className =? "qemu"				--> doFloat
	, className =? "qemu-system-x86_64"	--> doFloat
    , resource  =? "kdesktop"       	--> doIgnore
    , resource  =? "desktop_window" 	--> doIgnore
    ]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook xmproc = dynamicLogWithPP $ xmobarPP
		{ ppOutput = hPutStrLn xmproc
		, ppTitle = xmobarColor "green" "" . shorten 75
		}
 
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
	xmproc <- spawnPipe "xmobar"
	xmonad $ defaults xmproc
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults xmproc = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = avoidStruts  $  myLayout,
        manageHook         = manageDocks <+> myManageHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
    }
