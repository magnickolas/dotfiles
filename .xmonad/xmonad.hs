import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import System.Exit (exitSuccess)
import XMonad (Default (def), KeyMask, KeySym, Layout, X,
    XConfig (XConfig, focusFollowsMouse, keys, borderWidth, layoutHook, logHook,
    manageHook, modMask, startupHook, terminal, workspaces), io, kill, sendMessage, spawn,
    windows, withFocused, xmonad, (.|.), title, (=?), (<+>), className, handleEventHook)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (PP (ppCurrent, ppHidden, ppOutput, ppSep,
    ppSort, ppTitle, ppUrgent, ppVisible, ppWsSep), dynamicLogWithPP, shorten, wrap)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), avoidStruts, manageDocks, docks)
import XMonad.Layout (Tall (Tall), (|||), ChangeLayout (NextLayout))
import XMonad.Layout.Decoration (Theme (activeBorderColor, activeColor,
    activeTextColor, decoHeight, fontName, inactiveBorderColor, inactiveColor,
    inactiveTextColor, urgentBorderColor, urgentColor, urgentTextColor), fi, shrinkText)
import qualified XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.TabBarDecoration (XPPosition (Top), resizeVertical, tabBar)
import qualified XMonad.StackSet as W
import XMonad.Util.Image (Placement (CenterLeft, CenterRight))
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedScratchpad (namedScratchpadAction, namedScratchpadManageHook,
    NamedScratchpad (NS), defaultFloating, namedScratchpadFilterOutWorkspace, customFloating)
import XMonad.Prompt.Window (windowPrompt, WindowPrompt (Goto, Bring), wsWindows, allWindows)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt (font, autoComplete)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.ManageHook
    ( composeAll, (<&&>), resource, doFloat, (-->), doShift )

main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  spawn setupKeyboard
  --spawn setupMonitor
  runXMonad dbus

runXMonad dbus =
  xmonad $ docks $ ewmh
    desktopConfig
      { terminal          = myTerminal,
        modMask           = mod4Mask,
        borderWidth       = 0,
        focusFollowsMouse = False,
        startupHook = do
          spawnOnce clipboardManager
          spawnOnce screenshoter
          spawnOnce compositor
          spawn myBar,
        handleEventHook = handleEventHook def <+> fullscreenEventHook,
        layoutHook = myLayoutHook,
        keys       = \c -> myKKeys c `M.union` keys desktopConfig c,
        logHook    = dynamicLogWithPP (myLogHook dbus),
        manageHook = myManageHook,
        workspaces = myWorkspaces
      }

myManageHook = composeAll [
        (className =? "firefox" <&&> resource =? "Dialog") --> doFloat,
        (title =? "Telegram") --> doShift (myWorkspaces !! 8)
    ] <+> manageDocks <+> namedScratchpadManageHook myScratchpads

myScratchpads =
    [NS scratchpadTerminalTitle (myTerminal ++ " -t " ++ scratchpadTerminalTitle ++ scratchpadTerminalAdditionalOptions) (title =? scratchpadTerminalTitle) centerWin,
     NS spotifyQt spotifyQt (className =? spotifyQt) centerWin ]
    where
    centerWin = customFloating $ W.RationalRect (1/16) (1/16) (7/8) (7/8)

scratchpadTerminalTitle = "scratchpad_terminal"
scratchpadTerminalAdditionalOptions = " -o window.opacity=1.0 "

toggleSP = namedScratchpadAction myScratchpads

myKKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKKeys conf@(XConfig {modMask = modMask}) =
  M.fromList $
    [ ((modMask, xK_Return),               spawn myTerminal),
      ((modMask .|. shiftMask, xK_Return), windows W.swapMaster),
      ((modMask, xK_d),                    spawn dmenu),
      ((modMask, xK_i),                    spawn dmenuApp),
      ((controlMask .|. shiftMask, xK_4),  spawn takeScreenshot),
      ((modMask .|. shiftMask, xK_t),      withFocused toggleBorder),
      ((modMask .|. shiftMask, xK_u),      spawn suspend),
      ((modMask .|. shiftMask, xK_q),      kill),
      ((mod1Mask .|. shiftMask, xK_e),      confirmPrompt def { font = myFont } "exit" $ io exitSuccess),
      ((modMask, xK_f),                    sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),
      ((modMask, xK_w),                    sendMessage NextLayout),
      ((modMask, xK_o),                    safeSpawn browser []),
      ((modMask, xK_minus),                toggleSP scratchpadTerminalTitle),
      ((modMask, xK_equal),                toggleSP spotifyQt),
      ((modMask, xK_g), windowPrompt
           def { font = myFont, autoComplete = Just 0 }
           Goto allWindows),
      ((0, xF86XK_AudioMute),              spawn audioToggle),
      ((0, xF86XK_AudioRaiseVolume),       spawn raiseVolume),
      ((0, xF86XK_AudioLowerVolume),       spawn lowerVolume),
      ((0, xF86XK_MonBrightnessUp),        spawn brightnessUp),
      ((0, xF86XK_MonBrightnessDown),      spawn brightnessDown)
    ] ++ [
        ((modMask, key), windows $ W.greedyView ws)
        | (key,ws) <- myExtraWorkspaces
      ] ++ [
        ((modMask .|. shiftMask, key), windows $ W.shift ws)
        | (key,ws) <- myExtraWorkspaces
      ]

myExtraWorkspaces = [(xK_0, "[0]")]
myWorkspaces = ["DEV [1]","WWW [2]","[3]","[4]","[5]","[6]","[7]","[8]","IM [9]"] ++ map snd myExtraWorkspaces

myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput  = dbusOutput dbus,
      ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}",
      ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}",
      ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}",
      ppHidden  = wrap " " " ",
      ppWsSep   = "",
      ppSep     = " | ",
      ppTitle   = shorten 40,
      ppSort    = fmap
                    (namedScratchpadFilterOutWorkspace.)
                    (ppSort def)
    }

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myFontSize s = "xft:Terminus-" ++ show s ++ ":style=bold"
myFont = myFontSize 12

baseTheme :: Theme
baseTheme =
  def
    { activeColor         = base03,
      activeBorderColor   = base03,
      activeTextColor     = base01,
      inactiveBorderColor = base02,
      inactiveColor       = base02,
      inactiveTextColor   = base01,
      urgentColor         = yellow,
      urgentBorderColor   = yellow,
      urgentTextColor     = base02,
      fontName            = myFont,
      decoHeight          = 20
    }

tabTheme :: Theme
tabTheme =
  baseTheme
    {
      activeColor       = base00,
      activeBorderColor = base03,
      activeTextColor   = "#ffffff"
    }

myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = mySpacing' 4 (noBorders tiled) ||| noBorders tabs
      where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1 / 2
        delta   = 2 / 100
        tabs    = renamed [Replace "Tabs"] $ addTabs Simplest
        addTabs l =
          tabBar shrinkText tabTheme Top $
            resizeVertical (fi $ decoHeight tabTheme) l

-- Constants
myTerminal       = "alacritty"
clipboardManager = "parcellite"
compositor       = "picom"
screenshoter     = "flameshot"
takeScreenshot   = screenshoter ++ " gui"
dmenu            = "dmenu_run"
dmenuApp         = "j4-dmenu-desktop"
audioToggle      = "pactl set-sink-mute 0 toggle"
raiseVolume      = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
lowerVolume      = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
brightnessUp     = "~/scripts/change_brightness.sh inc"
brightnessDown   = "~/scripts/change_brightness.sh dec"
setupKeyboard    = "~/scripts/setup_keyboard.sh"
setupMonitor     = "~/scripts/setup_monitor.sh"
suspend          = "systemctl suspend"
myBar            = "~/.config/polybar/launch.sh"
browser          = "firefox"
spotifyQt        = "spotify-qt"

-- Colors
base00    = "#657b83"
base01    = "#586e75"
base02    = "#073642"
base03    = "#002b36"
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#504945"
bg3       = "#665c54"
bg4       = "#7c6f64"
green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
