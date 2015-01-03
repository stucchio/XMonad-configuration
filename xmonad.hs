import XMonad
import XMonad.Config.Gnome
import Data.Monoid
import System.Exit
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS

import XMonad.Prompt
import XMonad.Prompt.Shell

-- Layouts
import XMonad.Hooks.ManageHelpers
import XMonad.Config.Desktop (desktopLayoutModifiers)

import qualified XMonad.StackSet as W

import qualified Data.Map        as M
import Control.Monad
import Data.Monoid (All (All))

myModMask = mod4Mask
noKeyModifier = 0

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_Left), prevWS )
    , ((modm, xK_Right), nextWS )
    -- Gnome-do
    , ((noModMask         , xK_F1    ), spawn "gnome-do")
    -- close focused window
    , ((mod1Mask,           xK_F4     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((mod1Mask,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
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
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayout = Mirror tiled ||| Full ||| tiled
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

myManageHook = composeAll
               [ className =? "Gimp"      --> doFloat
               , resource =? "Do"         --> doIgnore
               , manageHook gnomeConfig
               , title =? "foo" --> doShift "2"
               , isFullscreen --> doFullFloat
               , className =? "Unity-2d-launcher" --> doFloat
               , className =? "Unity-2d-panel" --> doIgnore
               ]

newManageHook = myManageHook <+> manageHook defaultConfig

-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

totemEventHook :: Event -> X All
totemEventHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win
  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2
      -- The ATOM property type for changeProperty
      ptype = 4
      action = head dat
  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win
  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

evHook _ = return $ All True

main = xmonad $ gnomeConfig { terminal     = "gnome-terminal"
                            , modMask      = myModMask
                            , keys         = myKeys
                            , layoutHook   = ( desktopLayoutModifiers myLayout )
                            , manageHook = newManageHook
                            , startupHook = setWMName "LG3D"
                            , handleEventHook = totemEventHook
                            }