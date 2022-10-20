{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Config
import Control.Monad (when)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Foldable (find)
import Data.List ((\\), intersperse)
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
    (<||>),
    (=?),
 )
import XMonad.Actions.CopyWindow (copy, killAllOtherCopies)
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop (desktopConfig)
import qualified XMonad.Core
import XMonad.Hooks.DynamicLog (
    PP (
        ppCurrent,
        ppHidden,
        ppOutput,
        ppSep,
        ppSort,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
    ),
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
    isFloat,
    refocusLastLayoutHook,
    refocusLastWhen,
    refocusingIsActive,
    shiftRLWhen,
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
    Theme (
        decoHeight
    ),
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
import XMonad.Layout.StateFull (FocusTracking (FocusTracking))
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
import XMonad.Prompt.Pass (passOTPPrompt)
import XMonad.Prompt (
    XPConfig (searchPredicate),
    XPrompt (commandToComplete, nextCompletion, showXPrompt),
    getNextCompletion,
    mkXPrompt,
 )
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
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

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
                            { terminal = myTerminal cfg
                            , modMask = mod4Mask
                            , rootMask = rootMask def .|. pointerMotionMask
                            , borderWidth = 0
                            , focusFollowsMouse = True
                            , startupHook =
                                do
                                    spawnOnce $ clipboardManager cfg
                                    spawnOnce $ screenshoter cfg
                                    spawnOnce $ compositor cfg
                                    spawnOnce $ udiskie cfg
                                    spawnOnce $ screensaverBg cfg
                                    spawn $ setupKeyboard cfg
                                    spawn $ setWallpaper cfg
                                    spawn $ myBar cfg
                            , handleEventHook =
                                handleEventHook def
                                    <+> multiScreenFocusHook
                                    <+> refocusLastWhen (refocusingIsActive <||> isFloat)
                            , layoutHook =
                                myLayoutHook
                            , keys = refocusLastKeys <+> myKKeys <+> keys desktopConfig
                            , logHook =
                                updatePointer (0.25, 0.25) (0, 0)
                                    <+> nsHideOnFocusLoss myScratchpads
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

screenNum :: X CInt
screenNum = do
    xrandrOutput <- runProcessWithInput "xrandr" [] []
    grepOutput <- runProcessWithInput "grep" [" connected"] xrandrOutput
    (return . fromIntegral . length . lines) grepOutput

myRandrChangeHook :: X ()
myRandrChangeHook = do
    screens <- screenNum
    case screens of
        1 -> spawn $ setupDualMonitors cfg
        _ -> screenLayoutPrompt

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
            , (className =? "flameshot") --> centerWin
            , (title =? "webcam") --> webCam
            , (title =? "Telegram") --> doShift (myWorkspaces !! 8)
            , isDialog --> doF W.swapUp
            ]
        <+> manageDocks
        <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS
        (scratchpadTerminalTitle cfg)
        ( myTerminal cfg
            ++ " -t "
            ++ scratchpadTerminalTitle cfg
            ++ scratchpadTerminalAdditionalOptions cfg
        )
        (title =? scratchpadTerminalTitle cfg)
        centerWinBig
    , NS (spotifyQt cfg) (spotifyQt cfg) (className =? spotifyQt cfg) centerWin
    ]

dynamicLogWithPPUTF8 :: PP -> X ()
dynamicLogWithPPUTF8 pp =
    dynamicLogString pp
        >>= (io . ppOutput pp) . UTF8.encodeString

centerWin :: XMonad.Core.ManageHook
centerWin = customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

webCam :: XMonad.Core.ManageHook
webCam = customFloating $ W.RationalRect (3 / 4) (3 / 4 - 1 / 9) (1 / 4) (1 / 4)

centerWinBig :: XMonad.Core.ManageHook
centerWinBig = customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)

toggleSP :: String -> X ()
toggleSP = namedScratchpadAction myScratchpads

myKKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKKeys XConfig{modMask = winMask} =
    M.fromList $
        [ ((winMask, xK_Return), spawn $ myTerminal cfg)
        , ((winMask .|. shiftMask, xK_Return), windows W.swapMaster)
        , ((winMask, xK_d), spawn $ dmenu cfg)
        , ((winMask, xK_i), spawn $ dmenuApp cfg)
        , ((controlMask .|. shiftMask, xK_4), spawn $ takeScreenshot cfg)
        , ((winMask .|. shiftMask, xK_s), spawn $ takeScreenshot cfg)
        , ((controlMask .|. shiftMask, xK_3), spawn $ screenZoomer cfg)
        , ((winMask .|. shiftMask, xK_t), withFocused toggleBorder)
        , ((winMask .|. shiftMask, xK_u), spawn $ suspend cfg)
        , ((winMask .|. shiftMask, xK_x), spawn (switchLayout US) <+> spawn (screensaver cfg))
        , ((winMask .|. shiftMask, xK_q), kill)
        ,
            ( (mod1Mask .|. shiftMask, xK_e)
            , confirmPrompt promptConf "exit" $ io exitSuccess
            )
        ,
            ( (winMask, xK_f)
            , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
            )
        , ((winMask, xK_o), safeSpawn (browser cfg) [])
        , ((winMask, xK_minus), toggleSP $ scratchpadTerminalTitle cfg)
        , ((winMask, xK_equal), toggleSP $ spotifyQt cfg)
        , ((winMask, xK_g), spawn $ gotoWindow cfg)
        , ((winMask, xK_b), windowPrompt promptConf Bring allWindows)
        , ((winMask, xK_x), spawn (switchLayout US) <+> screenLayoutPrompt)
        , ((winMask, xK_p), spawn $ passPrompt cfg)
        , ((winMask .|. controlMask, xK_p), passOTPPrompt promptConf)
        , ((winMask .|. shiftMask, xK_m), spawn (setupKeyboard cfg) <+> spawn (setupMonitor cfg) <+> spawn (setWallpaper cfg))
        , ((winMask, xK_Left), sendMessage Shrink)
        , ((winMask, xK_Up), sendMessage MirrorExpand)
        , ((winMask, xK_Right), sendMessage Expand)
        , ((winMask, xK_Down), sendMessage MirrorShrink)
        , ((0, xF86XK_AudioMute), spawn $ audioToggle cfg)
        , ((0, xF86XK_AudioRaiseVolume), spawn $ raiseVolume cfg)
        , ((0, xF86XK_AudioLowerVolume), spawn $ lowerVolume cfg)
        , ((0, xF86XK_MonBrightnessUp), spawn $ brightnessUp cfg)
        , ((0, xF86XK_MonBrightnessDown), spawn $ brightnessDown cfg)
        , ((winMask, xK_a), sequence_ $ [windows $ copy i | i <- myWorkspaces \\ extraWorkspaces])
        , ((winMask .|. shiftMask, xK_a), killAllOtherCopies)
        , ((winMask, xK_n), nextScreen)
        , ((winMask .|. shiftMask, xK_n), shiftNextScreen)
        , ((winMask, xK_z), toggleWS)
        ]
            ++ [((winMask, key), windows $ W.greedyView ws) | (key, ws) <- extraWorkspacesBindings]
            ++ [ ((winMask .|. shiftMask, key), windows $ W.shift ws)
               | (key, ws) <- extraWorkspacesBindings
               ]
-- prompt
data SwitchLayout c where
    SwitchLayout :: (Eq c, Num c, Show c) => c -> SwitchLayout c
instance XPrompt (SwitchLayout c) where
    showXPrompt (SwitchLayout c) =
        "Layout [" ++ show c ++ " screen" ++ suf c ++ "]: "
      where
        suf 1 = ""
        suf _ = "s"
    commandToComplete _ c = c
    nextCompletion _ = getNextCompletion

screenLayoutPrompt :: X ()
screenLayoutPrompt = do
    c <- screenNum
    let t = SwitchLayout c
    mkXPrompt t conf compl action
  where
    conf = promptConf
    layouts = [horizontal, vertical, single]
    horizontal = "horizontal"
    vertical = "vertical"
    single = "single"
    action layout =
        spawn $ setupDualMonitors cfg ++ " --layout " ++ [head layout]
    compl s =
        return $ filter (searchPredicate conf s) layouts

extraWorkspaces :: [String]
extraWorkspaces = ["[0]"]

extraWorkspacesBindings :: [(KeySym, String)]
extraWorkspacesBindings = [xK_0] `zip` extraWorkspaces

myWorkspaces :: [String]
myWorkspaces =
    ["DEV [1]", "WWW [2]", "[3]", "[4]", "[5]", "[6]", "[7]", "[8]", "IM [9]"]
        ++ extraWorkspaces

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
        , ppCurrent = wrap ("%{B" ++ bg2 colors ++ "} ") " %{B-}"
        , ppVisible = wrap ("%{B" ++ bg1 colors ++ "} ") " %{B-}" . clickable
        , ppUrgent = wrap ("%{F" ++ red colors ++ "} ") " %{F-}" . clickable
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

spacing ::
    Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
spacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayoutHook =
    avoidStruts $ refocusLastLayoutHook $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = spacing 4 (noBorders tiled) ||| noBorders tabs
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
switchLayout :: LayoutT -> String
switchLayout = ("xkb-switch -s " ++) . show
