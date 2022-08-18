import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (when)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Foldable (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (All (All))
import Foreign.C (CInt)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import System.Exit (exitSuccess)
import XMonad (
    Default (def),
    Event (MotionEvent, ev_x, ev_y),
    Layout,
    Rectangle,
    ScreenDetail,
    ScreenId,
    WorkspaceId,
    X,
    XConfig (
        XConfig,
        borderWidth,
        focusFollowsMouse,
        keys,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        rootMask,
        startupHook,
        terminal,
        workspaces
    ),
    className,
    get,
    gets,
    handleEventHook,
    io,
    kill,
    rect_height,
    rect_width,
    rect_x,
    rect_y,
    screenRect,
    sendMessage,
    spawn,
    title,
    windows,
    windowset,
    withFocused,
    xmonad,
    (.|.),
    (<+>),
    (=?), (<||>),
 )
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Desktop (desktopConfig)
import qualified XMonad.Core
import XMonad.Hooks.DynamicLog (
    PP (ppCurrent, ppHidden, ppOutput, ppSep, ppSort, ppTitle, ppUrgent, ppVisible, ppWsSep),
    filterOutWsPP,
    shorten,
    wrap,
 )
import XMonad.Hooks.EwmhDesktops (
    ewmh,
    ewmhFullscreen,
 )
import XMonad.Hooks.InsertPosition (
    Focus (Newer),
    Position (Below),
    insertPosition,
 )
import XMonad.Hooks.ManageDocks (
    ToggleStruts (ToggleStruts),
    avoidStruts,
    docks,
    manageDocks,
 )
import XMonad.Hooks.ManageHelpers (
    isDialog,
 )
import XMonad.Hooks.RefocusLast (
    refocusLastLayoutHook,
    refocusLastWhen,
    refocusingIsActive,
    shiftRLWhen, isFloat,
 )
import XMonad.Hooks.Rescreen (
    RescreenConfig,
    afterRescreenHook,
    randrChangeHook,
    rescreenHook,
 )
import XMonad.Hooks.StatusBar.PP (dynamicLogString)
import XMonad.Layout (
    Resize (Expand, Shrink),
    (|||),
 )
import XMonad.Layout.Decoration (
    Theme (activeBorderColor, activeColor, activeTextColor, decoHeight, fontName, inactiveBorderColor, inactiveColor, inactiveTextColor, urgentBorderColor, urgentColor, urgentTextColor),
    fi,
    shrinkText,
 )
import qualified XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (
    MirrorResize (MirrorExpand, MirrorShrink),
    ResizableTall (ResizableTall),
 )
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (
    Border (Border),
    Spacing,
    spacingRaw,
 )
import XMonad.Layout.TabBarDecoration (
    XPPosition (Top),
    resizeVertical,
    tabBar,
 )
import XMonad.ManageHook (
    composeAll,
    doF,
    doShift,
    resource,
    (-->),
    (<&&>),
 )
import XMonad.Prompt (autoComplete, font)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Window (
    WindowPrompt (Bring),
    allWindows,
    windowPrompt,
 )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (
    NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    nsHideOnFocusLoss,
    scratchpadWorkspaceTag,
 )
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Actions.CopyWindow (copy, killAllOtherCopies)
import XMonad.Layout.StateFull (FocusTracking (FocusTracking))

main :: IO ()
main = do
    dbus <- D.connectSession
    D.requestName
        dbus
        (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    runXMonad dbus

runXMonad :: D.Client -> IO ()
runXMonad dbus =
    xmonad $
        rescreenHook rescreenCfg $
            docks $
                ewmhFullscreen $
                    ewmh
                        desktopConfig
                            { terminal = myTerminal
                            , modMask = mod4Mask
                            , rootMask = rootMask def .|. pointerMotionMask
                            , borderWidth = 0
                            , focusFollowsMouse = True
                            , startupHook =
                                do
                                    spawnOnce clipboardManager
                                    spawnOnce screenshoter
                                    spawnOnce compositor
                                    spawnOnce udiskie
                                    spawnOnce screensaverBg
                                    spawn setupKeyboard
                                    spawn setWallpaper
                                    spawn myBar
                            , handleEventHook =
                                handleEventHook def
                                    <+> multiScreenFocusHook
                                    <+> refocusLastWhen (refocusingIsActive <||> isFloat)
                            , layoutHook =
                                myLayoutHook
                            , keys = refocusLastKeys <+> myKKeys <+> keys desktopConfig
                            , logHook =
                                nsHideOnFocusLoss myScratchpads
                                    <+> (dynamicLogWithPPUTF8 . filterOutWsPP [scratchpadWorkspaceTag])
                                        (myLogHook dbus)
                            , manageHook = myManageHook
                            , workspaces = myWorkspaces
                            }

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent{ev_x = x, ev_y = y} = do
    ms <- getScreenForPos x y
    case ms of
        Just cursorScreen -> do
            let cursorScreenID = W.screen cursorScreen
            focussedScreenID <- gets (W.screen . W.current . windowset)
            when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
            return (All True)
        _ -> return (All True)
  where
    getScreenForPos ::
        CInt ->
        CInt ->
        X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
    getScreenForPos sx sy = do
        ws <- windowset <$> get
        let screens = W.current ws : W.visible ws
            inRects = map (inRect sx sy . screenRect . W.screenDetail) screens
        return $ fst <$> find snd (zip screens inRects)
    inRect :: CInt -> CInt -> Rectangle -> Bool
    inRect rx ry rect =
        let l = fromIntegral (rect_x rect)
            r = l + fromIntegral (rect_width rect)
            t = fromIntegral (rect_y rect)
            b = t + fromIntegral (rect_height rect)
         in rx >= l && rx < r && ry >= t && ry < b
    focusWS :: WorkspaceId -> X ()
    focusWS wsid = windows (W.view wsid)
multiScreenFocusHook _ = return (All True)

myAfterRescreenHook :: X ()
myAfterRescreenHook = return ()

myRandrChangeHook :: X ()
myRandrChangeHook = spawn setupMonitor

rescreenCfg :: RescreenConfig
rescreenCfg =
    def
        { afterRescreenHook = myAfterRescreenHook
        , randrChangeHook = myRandrChangeHook
        }

refocusLastKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
refocusLastKeys cnf =
    M.fromList $
        [ ( (modMask cnf .|. shiftMask, n)
          , windows =<< shiftRLWhen refocusingIsActive wksp
          )
        | (n, wksp) <- zip [xK_1 .. xK_9] (workspaces cnf)
        ]

myManageHook :: XMonad.Core.ManageHook
myManageHook =
    insertPosition Below Newer
        <+> composeAll
            [ (className =? "firefox" <&&> resource =? "Dialog") --> centerWin
            , (className =? "Pavucontrol") --> centerWin
            , (title =? "Telegram") --> doShift (myWorkspaces !! 8)
            , isDialog --> doF W.swapUp
            ]
        <+> manageDocks
        <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS
        scratchpadTerminalTitle
        ( myTerminal
            ++ " -t "
            ++ scratchpadTerminalTitle
            ++ scratchpadTerminalAdditionalOptions
        )
        (title =? scratchpadTerminalTitle)
        centerWinBig
    , NS spotifyQt spotifyQt (className =? spotifyQt) centerWin
    ]

dynamicLogWithPPUTF8 :: PP -> X ()
dynamicLogWithPPUTF8 pp =
    dynamicLogString pp
        >>= (io . ppOutput pp) . UTF8.encodeString

centerWin :: XMonad.Core.ManageHook
centerWin = customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

centerWinBig :: XMonad.Core.ManageHook
centerWinBig = customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)

scratchpadTerminalTitle :: String
scratchpadTerminalTitle = "scratchpad_terminal"

scratchpadTerminalAdditionalOptions :: String
scratchpadTerminalAdditionalOptions = " -o window.opacity=1.0 "

toggleSP :: String -> X ()
toggleSP = namedScratchpadAction myScratchpads

myKKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKKeys XConfig{modMask = winMask} =
    M.fromList $
        [ ((winMask, xK_Return), spawn myTerminal)
        , ((winMask .|. shiftMask, xK_Return), windows W.swapMaster)
        , ((winMask, xK_d), spawn dmenu)
        , ((winMask, xK_i), spawn dmenuApp)
        , ((controlMask .|. shiftMask, xK_4), spawn takeScreenshot)
        , ((winMask .|. shiftMask, xK_s), spawn takeScreenshot)
        , ((controlMask .|. shiftMask, xK_3), spawn screenZoomer)
        , ((winMask .|. shiftMask, xK_t), withFocused toggleBorder)
        , ((winMask .|. shiftMask, xK_u), spawn suspend)
        , ((winMask .|. shiftMask, xK_x), spawn (switchLayout US) <+> spawn screensaver)
        , ((winMask .|. shiftMask, xK_q), kill)
        ,
            ( (mod1Mask .|. shiftMask, xK_e)
            , confirmPrompt def{font = myFont} "exit" $ io exitSuccess
            )
        ,
            ( (winMask, xK_f)
            , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
            )
        , ((winMask, xK_o), safeSpawn browser [])
        , ((winMask, xK_minus), toggleSP scratchpadTerminalTitle)
        , ((winMask, xK_equal), toggleSP spotifyQt)
        , ((winMask, xK_g), spawn gotoWindow)
        , ((winMask, xK_b), windowPrompt def{font = myFont, autoComplete = Just 0} Bring allWindows)
        , ((winMask .|. shiftMask, xK_m), spawn setupKeyboard <+> spawn setupMonitor <+> spawn setWallpaper)
        , ((winMask, xK_Left), sendMessage Shrink)
        , ((winMask, xK_Up), sendMessage MirrorExpand)
        , ((winMask, xK_Right), sendMessage Expand)
        , ((winMask, xK_Down), sendMessage MirrorShrink)
        , ((0, xF86XK_AudioMute), spawn audioToggle)
        , ((0, xF86XK_AudioRaiseVolume), spawn raiseVolume)
        , ((0, xF86XK_AudioLowerVolume), spawn lowerVolume)
        , ((0, xF86XK_MonBrightnessUp), spawn brightnessUp)
        , ((0, xF86XK_MonBrightnessDown), spawn brightnessDown)
        , ((winMask, xK_a), sequence_ $ [windows $ copy i | i <- myWorkspaces])
        , ((winMask .|. shiftMask , xK_a), killAllOtherCopies)
        , ((winMask, xK_Down), nextWS)
        , ((winMask, xK_Up), prevWS)
        , ((winMask .|. shiftMask, xK_Down), shiftToNext >> nextWS)
        , ((winMask .|. shiftMask, xK_Up), shiftToPrev >> prevWS)
        , ((winMask, xK_Right), nextScreen)
        , ((winMask, xK_Left), prevScreen)
        , ((winMask .|. shiftMask, xK_Right), shiftNextScreen)
        , ((winMask .|. shiftMask, xK_Left), shiftPrevScreen)
        , ((winMask, xK_z), toggleWS)
        ]
            ++ [((winMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces]
            ++ [ ((winMask .|. shiftMask, key), windows $ W.shift ws)
               | (key, ws) <- myExtraWorkspaces
               ]

myExtraWorkspaces :: [(KeySym, String)]
myExtraWorkspaces = [(xK_0, "[0]")]

myWorkspaces :: [String]
myWorkspaces =
    ["DEV [1]", "WWW [2]", "[3]", "[4]", "[5]", "[6]", "[7]", "[8]", "IM [9]"]
        ++ map snd myExtraWorkspaces

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable :: [Char] -> [Char]
clickable ws = "%{A1:xdotool key super+" ++ show (index idx) ++ ":}" ++ ws ++ "%{A}"
  where
    idx = fromJust $ M.lookup ws myWorkspaceIndices
    index i
        | i == 10 = 0
        | otherwise = i

myLogHook :: D.Client -> PP
myLogHook dbus =
    def
        { ppOutput = dbusOutput dbus
        , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
        , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}" . clickable
        , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}" . clickable
        , ppHidden = wrap " " " " . clickable
        , ppWsSep = ""
        , ppSep = " | "
        , ppTitle = shorten 80
        , ppSort = ppSort def
        }

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal =
            (D.signal objectPath interfaceName memberName)
                { D.signalBody = [D.toVariant $ UTF8.decodeString str]
                }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

mySpacing' ::
    Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myFontSize :: Show a => a -> [Char]
myFontSize s = "xft:TerminessTTF Nerd Font Mono-" ++ show s ++ ":style=bold"

myFont :: [Char]
myFont = myFontSize (12 :: Integer)

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
    avoidStruts $ refocusLastLayoutHook $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = mySpacing' 4 (noBorders tiled) ||| noBorders tabs
      where
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio = 1 / 2
        delta = 2 / 100
        tabs = renamed [Replace "Tabs"] $ addTabs (FocusTracking Nothing Simplest)
        addTabs l =
            tabBar shrinkText tabTheme Top $
                resizeVertical (fi $ decoHeight tabTheme) l

data LayoutT = US | RU

instance Show LayoutT where
    show layout = case layout of
        US -> "us"
        RU -> "ru"

-- Commands
audioToggle = "pactl set-sink-mute 0 toggle"
brightnessDown = "~/scripts/change_brightness.sh dec"
brightnessUp = "~/scripts/change_brightness.sh inc"
browser = "firefox"
clipboardManager = "parcellite"
compositor = "picom"
dmenu = "dmenu_run"
dmenuApp = "rofi -show combi -combi-modi 'window,run,ssh,drun' -modi combi -show-icons"
gotoWindow = "rofi run -show"
lockScreen = "~/scripts/lockscreen.sh"
lowerVolume = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
myBar = "~/.config/polybar/launch.sh"
myTerminal = "alacritty"
raiseVolume = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
screenZoomer = "boomer"
screenshoter = "flameshot"
screensaverBg = "xscreensaver -no-splash"
screensaver = "xscreensaver-command -lock"
setWallpaper = "~/scripts/set_wallpaper.sh"
setupKeyboard = "~/scripts/setup_keyboard.sh"
setupMonitor = "~/scripts/setup_monitor.sh"
spotifyQt = "spotify-qt"
suspend = "systemctl suspend"
switchLayout :: LayoutT -> String
switchLayout = ("xkb-switch -s " ++) . show
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
