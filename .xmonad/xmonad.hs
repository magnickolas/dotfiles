import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import System.Exit (exitSuccess)
import XMonad
  ( Default(def)
  , KeyMask
  , KeySym
  , Layout
  , X
  , XConfig(XConfig, borderWidth, focusFollowsMouse, keys, layoutHook,
        logHook, manageHook, modMask, startupHook, terminal, workspaces)
  , (.|.)
  , (<+>)
  , (=?)
  , className
  , handleEventHook
  , io
  , kill
  , sendMessage
  , spawn
  , title
  , windows
  , withFocused
  , xmonad
  )
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
  ( PP(ppCurrent, ppHidden, ppOutput, ppSep, ppSort, ppTitle, ppUrgent,
   ppVisible, ppWsSep)
  , dynamicLogWithPP
  , filterOutWsPP
  , shorten
  , wrap
  )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
  ( ToggleStruts(ToggleStruts)
  , avoidStruts
  , docks
  , manageDocks
  )
import XMonad.Hooks.RefocusLast
  ( refocusLastLayoutHook
  , refocusLastLogHook
  , refocusLastWhen
  , refocusWhen
  , refocusingIsActive
  , shiftRLWhen
  , toggleFocus
  )
import XMonad.Layout (ChangeLayout(NextLayout), Tall(Tall), (|||))
import XMonad.Layout.Decoration
  ( Theme(activeBorderColor, activeColor, activeTextColor, decoHeight,
      fontName, inactiveBorderColor, inactiveColor, inactiveTextColor,
      urgentBorderColor, urgentColor, urgentTextColor)
  , fi
  , shrinkText
  )
import qualified XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (EOT(EOT), (??), mkToggle)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers(MIRROR, NBFULL, NOBORDERS)
  )
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.Spacing (Border(Border), Spacing, spacingRaw)
import XMonad.Layout.TabBarDecoration (XPPosition(Top), resizeVertical, tabBar)
import XMonad.Layout.TrackFloating (trackFloating, useTransientFor)
import XMonad.ManageHook ((-->), (<&&>), composeAll, doFloat, doShift, resource)
import XMonad.Prompt (autoComplete, font)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window
  ( WindowPrompt(Bring, Goto)
  , allWindows
  , windowPrompt
  , wsWindows
  )
import qualified XMonad.StackSet as W
import XMonad.Util.Image (Placement(CenterLeft, CenterRight))
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad(NS)
  , customFloating
  , defaultFloating
  , namedScratchpadAction
  , namedScratchpadManageHook
  , nsHideOnFocusLoss
  , scratchpadWorkspaceTag
  )
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  spawn setupKeyboard
  spawn setupMonitor
  spawn setWallpaper
  runXMonad dbus

runXMonad dbus =
  xmonad $
  docks $
  ewmhFullscreen $
  ewmh
    desktopConfig
      { terminal = myTerminal
      , modMask = mod4Mask
      , borderWidth = 0
      , focusFollowsMouse = False
      , startupHook =
          do spawnOnce clipboardManager
             spawnOnce screenshoter
             spawnOnce compositor
             spawnOnce udiskie
             spawn myBar
      , handleEventHook =
          refocusLastWhen refocusingIsActive <+> handleEventHook def
      , layoutHook =
          refocusLastLayoutHook $ trackFloating (useTransientFor myLayoutHook)
      , keys = refocusLastKeys <+> myKKeys <+> keys desktopConfig
      , logHook =
          nsHideOnFocusLoss myScratchpads <+>
          (dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag])
            (myLogHook dbus)
      , manageHook = myManageHook
      , workspaces = myWorkspaces
      }

refocusLastKeys cnf =
  M.fromList $
  [ ( (modMask cnf .|. shiftMask, n)
    , windows =<< shiftRLWhen refocusingIsActive wksp)
  | (n, wksp) <- zip [xK_1 .. xK_9] (workspaces cnf)
  ]

myManageHook =
  composeAll
    [ (className =? "firefox" <&&> resource =? "Dialog") --> centerWin
    , (className =? "Pavucontrol") --> centerWin
    , (title =? "Telegram") --> doShift (myWorkspaces !! 8)
    ] <+>
  manageDocks <+> namedScratchpadManageHook myScratchpads

myScratchpads =
  [ NS
      scratchpadTerminalTitle
      (myTerminal ++
       " -t " ++ scratchpadTerminalTitle ++ scratchpadTerminalAdditionalOptions)
      (title =? scratchpadTerminalTitle)
      centerWinBig
  , NS spotifyQt spotifyQt (className =? spotifyQt) centerWin
  ]

centerWin = customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

centerWinBig = customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)

scratchpadTerminalTitle = "scratchpad_terminal"

scratchpadTerminalAdditionalOptions = " -o window.opacity=1.0 "

toggleSP = namedScratchpadAction myScratchpads

myKKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKKeys conf@(XConfig {modMask = modMask}) =
  M.fromList $
  [ ((modMask, xK_Return), spawn myTerminal)
  , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((modMask, xK_d), spawn dmenu)
  , ((modMask, xK_i), spawn dmenuApp)
  , ((controlMask .|. shiftMask, xK_4), spawn takeScreenshot)
  , ((modMask .|. shiftMask, xK_t), withFocused toggleBorder)
  , ((modMask .|. shiftMask, xK_u), spawn suspend)
  , ((modMask .|. shiftMask, xK_q), kill)
  , ( (mod1Mask .|. shiftMask, xK_e)
    , confirmPrompt def {font = myFont} "exit" $ io exitSuccess)
  , ( (modMask, xK_f)
    , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  , ((modMask, xK_w), sendMessage NextLayout)
  , ((modMask, xK_o), safeSpawn browser [])
  , ((modMask, xK_minus), toggleSP scratchpadTerminalTitle)
  , ((modMask, xK_equal), toggleSP spotifyQt)
  , ( (modMask, xK_g)
    , windowPrompt def {font = myFont, autoComplete = Just 0} Goto allWindows)
  , ( (modMask, xK_b)
    , windowPrompt def {font = myFont, autoComplete = Just 0} Bring allWindows)
  , ((modMask .|. shiftMask, xK_x), spawn lockScreen <+> spawn suspend)
  , ((0, xF86XK_AudioMute), spawn audioToggle)
  , ((0, xF86XK_AudioRaiseVolume), spawn raiseVolume)
  , ((0, xF86XK_AudioLowerVolume), spawn lowerVolume)
  , ((0, xF86XK_MonBrightnessUp), spawn brightnessUp)
  , ((0, xF86XK_MonBrightnessDown), spawn brightnessDown)
  ] ++
  [((modMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces] ++
  [ ((modMask .|. shiftMask, key), windows $ W.shift ws)
  | (key, ws) <- myExtraWorkspaces
  ]

myExtraWorkspaces = [(xK_0, "[0]")]

myWorkspaces =
  ["DEV [1]", "WWW [2]", "[3]", "[4]", "[5]", "[6]", "[7]", "[8]", "IM [9]"] ++
  map snd myExtraWorkspaces

myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = shorten 40
    , ppSort = ppSort def
    }

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          {D.signalBody = [D.toVariant $ UTF8.decodeString str]}
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

mySpacing' ::
     Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myFontSize s = "xft:Terminus-" ++ show s ++ ":style=bold"

myFont = myFontSize 12

baseTheme :: Theme
baseTheme =
  def
    { activeColor = base03
    , activeBorderColor = base03
    , activeTextColor = base01
    , inactiveBorderColor = base02
    , inactiveColor = base02
    , inactiveTextColor = base01
    , urgentColor = yellow
    , urgentBorderColor = yellow
    , urgentTextColor = base02
    , fontName = myFont
    , decoHeight = 20
    }

tabTheme :: Theme
tabTheme =
  baseTheme
    { activeColor = base00
    , activeBorderColor = base03
    , activeTextColor = "#ffffff"
    }

myLayoutHook =
  avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = mySpacing' 4 (noBorders tiled) ||| noBorders tabs
      where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1 / 2
        delta = 2 / 100
        tabs = renamed [Replace "Tabs"] $ addTabs Simplest
        addTabs l =
          tabBar shrinkText tabTheme Top $
          resizeVertical (fi $ decoHeight tabTheme) l

-- Commands
audioToggle = "pactl set-sink-mute 0 toggle"

brightnessDown = "~/scripts/change_brightness.sh dec"

brightnessUp = "~/scripts/change_brightness.sh inc"

browser = "firefox"

clipboardManager = "parcellite"

compositor = "picom"

dmenu = "dmenu_run"

dmenuApp = "j4-dmenu-desktop"

lockScreen = "~/scripts/lockscreen.sh"

lowerVolume = "pactl set-sink-volume @DEFAULT_SINK@ -5%"

myBar = "~/.config/polybar/launch.sh"

myTerminal = "alacritty"

raiseVolume = "pactl set-sink-volume @DEFAULT_SINK@ +5%"

screenshoter = "flameshot"

setWallpaper = "~/scripts/set_wallpaper.sh"

setupKeyboard = "~/scripts/setup_keyboard.sh"

setupMonitor = "~/scripts/setup_monitor.sh"

spotifyQt = "spotify-qt"

suspend = "systemctl suspend"

takeScreenshot = screenshoter ++ " gui"

udiskie = "udiskie"

-- Colors
base00 = "#657b83"

base01 = "#586e75"

base02 = "#073642"

base03 = "#002b36"

fg = "#ebdbb2"

bg = "#282828"

gray = "#a89984"

bg1 = "#3c3836"

bg2 = "#504945"

bg3 = "#665c54"

bg4 = "#7c6f64"

green = "#b8bb26"

darkgreen = "#98971a"

red = "#fb4934"

darkred = "#cc241d"

yellow = "#fabd2f"

blue = "#83a598"

purple = "#d3869b"

aqua = "#8ec07c"
