{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wunused-binds -Wunused-do-bind -Wunused-foralls -Wunused-imports -Wunused-local-binds -Wunused-matches -Wunused-pattern-binds -Wunused-top-binds -Wunused-type-patterns #-}

import Config
import Control.Monad (when, (>=>))
import Data.Foldable (find)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Map.Lazy (size)
import Data.Monoid (All (All))
import Foreign.C (CInt)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import System.Exit (exitSuccess)
import XMonad
  ( Default (def),
    Event (MotionEvent, ev_x, ev_y),
    Layout,
    Rectangle,
    ScreenDetail,
    ScreenId,
    WorkspaceId,
    X,
    XConfig
      ( XConfig,
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
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, toggleWS)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Core (ManageHook)
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppHidden,
        ppLayout,
        ppOrder,
        ppSep,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
      ),
    filterOutWsPP,
    shorten,
    wrap,
    xmobarPP,
    xmobarRaw,
    xmonadPropLog,
  )
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    ewmhFullscreen,
  )
import XMonad.Hooks.InsertPosition
  ( Focus (Newer),
    Position (Below),
    insertPosition,
  )
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (ToggleStruts),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers
  ( isDialog,
  )
import XMonad.Hooks.RefocusLast
  ( isFloat,
    refocusLastLayoutHook,
    refocusLastWhen,
    refocusingIsActive,
    shiftRLWhen,
  )
import XMonad.Hooks.Rescreen
  ( RescreenConfig,
    afterRescreenHook,
    randrChangeHook,
    rescreenHook,
  )
import XMonad.Hooks.StatusBar.PP (dynamicLogString, xmobarColor)
import XMonad.Layout
  ( Resize (Expand, Shrink),
    (|||),
  )
import XMonad.Layout.Decoration
  ( Theme
      ( decoHeight
      ),
    fi,
    shrinkText,
  )
import XMonad.Layout.Hidden (hiddenWindows, hideWindow, popNewestHiddenWindow)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableTall (ResizableTall),
  )
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    spacingRaw,
  )
import XMonad.Layout.StateFull (FocusTracking (FocusTracking))
import XMonad.Layout.TabBarDecoration
  ( XPPosition (Top),
    resizeVertical,
    tabBar,
  )
import XMonad.ManageHook
  ( composeAll,
    doF,
    doShift,
    resource,
    (-->),
    (<&&>),
  )
import XMonad.Prompt
  ( XPConfig (searchPredicate),
    XPrompt (commandToComplete, nextCompletion, showXPrompt),
    getNextCompletion,
    mkXPrompt,
  )
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Pass (passOTPPrompt)
import XMonad.Prompt.Window
  ( WindowPrompt (Bring),
    allWindows,
    windowPrompt,
  )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    nsHideOnFocusLoss,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Run (runProcessWithInput, safeSpawnProg)
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main = do
  xmonad $
    rescreenHook rescreenCfg $
      docks $
        ewmhFullscreen $
          ewmh
            desktopConfig
              { terminal = terminal' cfg,
                modMask = mod4Mask,
                rootMask = rootMask def .|. pointerMotionMask,
                borderWidth = 0,
                focusFollowsMouse = True,
                startupHook =
                  do
                    spawnOnce $ kmonad cfg
                    spawnOnce $ kmonadKeycool cfg
                    spawnOnce $ xmobar cfg
                    spawnOnce $ compositor cfg
                    spawn $ setWallpaper cfg
                    spawnOnce $ clipboardManager cfg
                    spawnOnce $ screenshoter cfg
                    spawnOnce $ udiskie cfg
                    spawnOnce $ screensaverBg cfg
                    spawnOnce $ liveWallpaperServer cfg
                    spawnOnce $ rustLspMultiplex cfg
                    -- workaround fixing text insertion
                    -- https://github.com/jordansissel/xdotool/issues/49
                    spawnOnce "setxkbmap",
                handleEventHook =
                  handleEventHook def
                    <+> multiScreenFocusHook
                    <+> refocusLastWhen (refocusingIsActive <||> isFloat),
                layoutHook =
                  myLayoutHook,
                keys = refocusLastKeys <+> myKKeys <+> keys desktopConfig,
                logHook =
                  updatePointer (0.25, 0.25) (0, 0)
                    <+> nsHideOnFocusLoss
                      myScratchpads
                    <+> ((dynamicLogString >=> xmonadPropLog) . filterOutWsPP [scratchpadWorkspaceTag])
                      xmobarLogHook',
                manageHook = myManageHook,
                workspaces = myWorkspaces
              }

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent {ev_x = x, ev_y = y} = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focusedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focusedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
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
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }

refocusLastKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
refocusLastKeys cnf =
  M.fromList $
    [ ( (modMask cnf .|. shiftMask, n),
        windows =<< shiftRLWhen refocusingIsActive wksp
      )
      | (n, wksp) <- zip [xK_1 .. xK_9] (workspaces cnf)
    ]

myManageHook :: ManageHook
myManageHook =
  insertPosition Below Newer
    <+> composeAll
      [ (className =? "firefox" <&&> resource =? "Dialog") --> centerWin,
        (className =? "Pavucontrol") --> centerWin,
        (className =? "Zenity") --> centerWin,
        (className =? "flameshot") --> centerWin,
        (title =? "webcam") --> webCam,
        (title =? "Telegram") --> doShift (ws messenger),
        (className =? "obsidian") --> doShift (ws book),
        {- requires https://github.com/dasJ/spotifywm since spotify sets
        its class name too late for xmonad to catch it.
        It can also be achieved through `onTitleChange`, but this way
        it's more comfortable in terms of screen flashing on running -}
        (className =? "Spotify") --> doShift (ws music),
        isDialog --> doF W.swapUp
      ]
    <+> manageDocks
    <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS
      (scratchpadClass cfg)
      (scratchpadTerminal cfg)
      (className =? scratchpadClass cfg)
      centerWinBig,
    NS (spotifyQt cfg) (spotifyQt cfg) (className =? spotifyQt cfg) centerWin
  ]

centerWin :: ManageHook
centerWin = customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

webCam :: ManageHook
webCam = customFloating $ W.RationalRect (3 / 4) (3 / 4 - 1 / 9) (1 / 4) (1 / 4)

centerWinBig :: ManageHook
centerWinBig = customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)

toggleSP :: String -> X ()
toggleSP = namedScratchpadAction myScratchpads

myKKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKKeys XConfig {modMask = winMask} =
  M.fromList $
    [ ((winMask, xK_Return), spawn $ terminal' cfg),
      ((winMask .|. shiftMask, xK_Return), windows W.swapMaster),
      ((winMask, xK_d), us <+> spawn (dmenu cfg)),
      ((winMask, xK_i), us <+> spawn (dmenuApp cfg)),
      ((controlMask .|. shiftMask, xK_4), spawn $ takeScreenshot cfg),
      ((winMask .|. shiftMask, xK_s), spawn $ takeScreenshot cfg),
      ((winMask .|. shiftMask, xK_c), us <+> spawn (xcolor cfg)),
      ((controlMask .|. shiftMask, xK_3), spawn $ screenZoomer cfg),
      ((winMask .|. shiftMask, xK_t), withFocused toggleBorder),
      ((winMask .|. shiftMask, xK_u), spawn $ suspend cfg),
      ((winMask .|. shiftMask, xK_x), us <+> spawn (screensaver cfg)),
      ((winMask .|. shiftMask, xK_q), kill),
      ((mod1Mask .|. shiftMask, xK_e), confirmPrompt promptConf "exit" $ io exitSuccess),
      ((mod1Mask, xK_space), spawn $ layoutSwitch cfg),
      ((winMask, xK_grave), spawn $ layoutExtra cfg),
      ((winMask, xK_f), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),
      ((winMask, xK_o), safeSpawnProg (browser cfg)),
      ((winMask, xK_minus), toggleSP $ scratchpadClass cfg),
      ((winMask, xK_equal), toggleSP $ spotifyQt cfg),
      ((winMask, xK_g), spawn $ gotoWindow cfg),
      ((winMask, xK_c), us <+> windowPrompt promptConf Bring allWindows),
      ((winMask, xK_x), us <+> screenLayoutPrompt),
      ((winMask, xK_p), us <+> spawn (passPromptType cfg)),
      ((winMask, xK_v), us <+> splitLayoutPrompt),
      ((winMask .|. shiftMask, xK_p), us <+> spawn (passPrompt cfg)),
      ((winMask, xK_e), us <+> spawn (emojiYank cfg)),
      ((winMask, xK_b), us <+> spawn (bookmarkChoose cfg)),
      ((winMask .|. shiftMask, xK_b), us <+> spawn (bookmarkAdd cfg)),
      ((winMask .|. controlMask, xK_p), us <+> passOTPPrompt promptConf),
      ((winMask, xK_Left), sendMessage Shrink),
      ((winMask, xK_Up), sendMessage MirrorExpand),
      ((winMask, xK_Right), sendMessage Expand),
      ((winMask, xK_Down), sendMessage MirrorShrink),
      ((0, xF86XK_AudioMute), spawn $ audioToggle cfg),
      ((0, xF86XK_AudioRaiseVolume), spawn $ raiseVolume cfg),
      ((0, xF86XK_AudioLowerVolume), spawn $ lowerVolume cfg),
      ((0, xF86XK_AudioPlay), spawn $ spotifyToggle cfg),
      ((0, xF86XK_AudioNext), spawn $ spotifyNext cfg),
      ((0, xF86XK_AudioPrev), spawn $ spotifyPrev cfg),
      ((0, xF86XK_MonBrightnessUp), spawn $ brightnessUp cfg),
      ((0, xF86XK_MonBrightnessDown), spawn $ brightnessDown cfg),
      ((winMask, xK_a), sequence_ $ [windows $ copy i | i <- myWorkspaces \\ extraWorkspaces]),
      ((winMask .|. shiftMask, xK_a), killAllOtherCopies),
      ((winMask, xK_n), nextScreen),
      ((winMask .|. shiftMask, xK_n), shiftNextScreen),
      ((winMask, xK_z), toggleWS),
      ((winMask, xK_backslash), withFocused hideWindow),
      ((winMask .|. shiftMask, xK_backslash), popNewestHiddenWindow)
    ]
      ++ [((winMask, key), windows $ W.greedyView ws) | (key, ws) <- extraWorkspacesBindings]
      ++ [ ((winMask .|. shiftMask, key), windows $ W.shift ws)
           | (key, ws) <- extraWorkspacesBindings
         ]

newtype SimplePrompt = SimplePrompt String

instance XPrompt SimplePrompt where
  showXPrompt (SimplePrompt s) = s
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

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

splitLayoutPrompt :: X ()
splitLayoutPrompt = do
  let t = SimplePrompt "Virtual split: "
  mkXPrompt t conf compl action
  where
    conf = promptConf
    layouts = [horizontal, vertical]
    horizontal = "horizontal"
    vertical = "vertical"
    action layout =
      spawn $ "~/scripts/vir.sh " ++ [head layout]
    compl s =
      return $ filter (searchPredicate conf s) layouts

extraWorkspacesBindings :: [(KeySym, String)]
extraWorkspacesBindings = [xK_0] `zip` extraWorkspaces

wsIdxKeymap :: Integer -> String
wsIdxKeymap i = show $ (i + 1) `mod` toInteger (size workspaceIconM)

clickableXmobar :: String -> String
clickableXmobar ws =
  "<fn=2><action=`xdotool key super+"
    ++ wsIdxKeymap (wsIdxByIcon ws)
    ++ "`>"
    ++ ws
    ++ "</action></fn>"

xmobarLogHook' :: PP
xmobarLogHook' =
  xmobarPP
    { ppCurrent =
        wrap
          (ppspace ++ "<box type=Full ml=7 mr=7 color=" ++ "#504945" ++ ">")
          ("</box>" ++ ppspace)
          . xmobarColor "#c3e88d" ""
          . clickableXmobar,
      ppHidden = wrap ppspace ppspace . clickableXmobar,
      ppVisible = wrap ppspace ppspace . xmobarColor "#c3e88d" "" . clickableXmobar,
      ppLayout = mempty,
      ppSep = "<fc=#666666><hspace=5/>| </fc>",
      ppWsSep = "",
      ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!",
      ppTitle = xmobarRaw . shorten 25,
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
    }
  where
    ppspace = "<hspace=10/>"

spacing ::
  Integer -> l a -> ModifiedLayout Spacing l a
spacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayoutHook =
  avoidStruts $ refocusLastLayoutHook $ hiddenWindows $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
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

us :: X ()
us = spawn (switchLayout US)
