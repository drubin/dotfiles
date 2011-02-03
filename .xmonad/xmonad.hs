{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
 
-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
-- 
-- Works on xmonad-0.8, NOT on 0.7 or below; and of course
-- xmonad-contrib needs to be installed as well
-- Pidgin and Skype windows are automatically placed onto the IM workspace.
-- Their contact lists each get a column on the right side of the screen,
-- and all their other windows (chat windows, etc.) go into a grid layout
-- in the remaining space.
-- (This uses a copied and modified version of XMonad.Layout.IM.)

-- Changed all keybindings so original comments are useless.

import IO
import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio
import Data.List
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout.MultiToggle  
import XMonad.Layout.MultiToggle.Instances

-- Exit stuff
import System.IO
import System.Exit
import XMonad.Hooks.SetWMName


myTerminal    = "/usr/bin/uxterm"
myStartupHook = setWMName "LG3D"


-- | Strip xmobar markup. Useful to remove ppHidden color from ppUrgent
--   field. For example:
--
-- >     , ppHidden          = xmobarColor "gray20" "" . wrap "<" ">"
-- >     , ppUrgent          = xmobarColor "dark orange" "" .  xmobarStrip
myXmobarStrip :: String -> String
myXmobarStrip = strip [] where
    strip keep x
      | null x                 = keep
      | "<fc="  `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
      | "</fc>" `isPrefixOf` x = strip keep (drop 5  x)
      | '<' == head x          = strip (keep ++ "<") (tail x)
      | otherwise              = let (good,x') = span (/= '<') x
                                 in strip (keep ++ good) x'

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = defaultConfig

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 1
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#A0A0D0"

-- workspaces
myWorkspaces = ["1", "web", "email","4","im","6","7","fullscreen","9"]
isFullscreen = (== "fullscreen")

-- layouts
basicLayout = smartBorders $ Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 6/10
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

myLayoutHook = fullscreen $ im $ normal where
    normal     = tallLayout ||| wideLayout ||| singleLayout
    fullscreen = onWorkspace "fullscreen" fullscreenLayout
    im         = onWorkspace "im" imLayout

-- special treatment for specific windows:
-- put the Pidgin and Skype windows in the im workspace
myManageHook = webManageHooks <+> imManageHooks <+> floatManageHooks <+> manageHook myBaseConfig <+> manageDocks <+> staloneManageHooks
floatManageHooks = composeAll [isFloat --> doFloat] where
    isFloat = foldr1 (<||>) [isDo, isGimp, isFirefoxDialog, isPidginDialog, isVMDdialog] where
        isDo   = className =? "Do"
        isGimp = className =? "Gimp-2.6..."
        isFirefoxDialog = className =? "FireFox" <&&> (resource =? "Browser" <||> resource =? "Toplevel")
        isPidginDialog  = className =? "Pidgin" <&&> foldr1 (<||>) [
            title =? "Accounts", title =? "System Log", title =? "Plugins",
            title =? "Room List", title =? "Custom Smiley Manager",
            title =? "File Transfers", title =? "Buddy Information"]
        isVMDdialog = title =? "Graphical Representations"
imManageHooks = composeAll [isIM --> moveToIM] where
    isIM     = foldr1 (<||>) [isPidgin, isSkype]
    isPidgin = className =? "Pidgin"
    isSkype  = className =? "Skype"
    moveToIM = doF $ S.shift "im"
webManageHooks = composeAll [isWeb --> moveToWeb] where
    isWeb     = className =? "FixFox"
    moveToWeb = doF $ S.shift "web"
staloneManageHooks = composeAll [isStalone --> doIgnore] where
    isStalone = resource =? "stalonetray"

-- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
 
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf )          -- launch a terminal
    , ((modm .|. shiftMask, xK_KP_Enter), spawn $ XMonad.terminal conf )
    , ((mod1Mask .|. controlMask, xK_space), spawn $ XMonad.terminal conf )     -- Needs to be mod1Mask as this is alt
    , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")    -- launch dmenu
    , ((modm .|. shiftMask, xK_c     ), kill)                                   -- close focused window 
    , ((modm, xK_space ), sendMessage NextLayout)                               -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)     --  Reset the layouts on the current workspace to default
    , ((modm, xK_n), refresh)                                                   -- Resize viewed windows to the correct size
	, ((modm, xK_s), spawn "scrot --select ~/Screenshots/%Y-%m-%d-%H%M%S.png" )	        -- Take a screenshot
    , ((modm, xK_Tab ), windows W.focusDown)                                    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)                                       -- Move focus to the next window
    , ((modm, xK_k), windows W.focusUp  )                                       -- Move focus to the previous window
    , ((modm,xK_m), windows W.focusMaster  )                                    -- Move focus to the master window
    , ((modm, xK_g), windows W.swapMaster)                                      -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )                        -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )                        -- Swap the focused window with the previous window
    , ((modm, xK_h), sendMessage Shrink)                                        -- Shrink the master area
    , ((modm, xK_l), sendMessage Expand)                                        -- Expand the master area
    , ((modm, xK_t), withFocused $ windows . W.sink)                            -- Push window back into tiling
    , ((modm, xK_comma), sendMessage (IncMasterN 1))                            -- Increment the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))                        -- Deincrement the number of windows in the master area
    , ((modm , xK_b ), sendMessage ToggleStruts)                                -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    , ((modm .|. shiftMask, xK_b), sendMessage $ Toggle NOBORDERS)              -- toggle borders	
	, ((modm, xK_Left), prevWS)                                                 -- Moving workspaces
	, ((modm, xK_Right), nextWS )
	, ((modm .|. shiftMask,	xK_Left	), shiftToPrev)
	, ((modm .|. shiftMask,	xK_Right),shiftToNext)
    , ((modm .|. shiftMask, xK_l ), spawn "xscreensaver-command -lock")         -- Lock the screen
	, ((modm .|. shiftMask,	xK_s	),	spawn "gksudo pm-suspend")             -- Suspend	
	, ((modm .|. shiftMask,	xK_h	),	spawn "suspend.sh Hibernate")           -- Hibernate
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))              -- Quit xmonad
    , ((modm, xK_q), restart "xmonad" True)                                     -- Restart xmonad, ie reload config
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
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


-- put it all together
main = do
    xmobar <- spawnPipe "xmobar" -- spawns xmobar and returns a handle
    xmonad $ withUrgencyHook NoUrgencyHook myBaseConfig
      { modMask = myModMask
      , terminal = myTerminal
      , workspaces = myWorkspaces
      , layoutHook = myLayoutHook
      , manageHook = myManageHook
      , borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , keys = myKeys
      , mouseBindings = myMouseBindings
      , startupHook   = myStartupHook
      -- print the output of xmobarPP to the handle
      , logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmobar
        , ppUrgent = xmobarColor "yellow" "red" . myXmobarStrip
        }
      }

-- modified version of XMonad.Layout.IM --

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    wrs <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return ((zip rosters rosterRects) ++ fst wrs, snd wrs)
