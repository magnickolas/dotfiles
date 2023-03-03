{-# OPTIONS_GHC -Wno-missing-signatures -Wunused-binds -Wunused-do-bind -Wunused-foralls -Wunused-imports -Wunused-local-binds -Wunused-matches -Wunused-pattern-binds -Wunused-top-binds -Wunused-type-patterns #-}

module Config (module Config) where

import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Tuple (swap)
import XMonad.Layout.Decoration
import XMonad.Prompt

cfg :: Config
cfg =
  Config
    { audioToggle = "pactl set-sink-mute 0 toggle",
      bookmarkAdd = "x bookmark add",
      bookmarkChoose = "~/.local/bin/x bookmark",
      brightnessDown = "x brightness dec",
      brightnessUp = "x brightness inc",
      browser = "firefox",
      clipboardManager = "parcellite",
      compositor = "picom",
      dmenu = "dmenu_run",
      dmenuApp = "rofi -show combi -combi-modi 'window,run,ssh,drun' -modi combi -show-icons",
      emojiYank = "x emoji",
      font' = "xft:Iosevka Term-14:style=term",
      gotoWindow = "rofi -modi window -show window -show-icons",
      kmonad = "kmonad ~/.config/kmonad/config_internal.kbd",
      kmonadKeycool = "kmonad ~/.config/kmonad/config_keycool.kbd",
      liveWallpaperServer = "x live-wallpaper run-server",
      lockScreen = "~/scripts/lockscreen.sh",
      lowerVolume = "pactl set-sink-volume @DEFAULT_SINK@ -5%",
      terminal' = "konsole",
      passPrompt = "passmenu",
      passPromptType = "passmenu --type",
      raiseVolume = "pactl set-sink-volume @DEFAULT_SINK@ +5%",
      rustLspMultiplex = "~/.cargo/bin/ra-multiplex-server",
      scratchpadClass = sClass,
      scratchpadTerminal = "konsole --name " ++ sClass,
      -- scratchpadTerminal = "wezterm start --class " ++ sClass,
      -- scratchpadTerminal = "alacritty -o window.opacity=1 --class " ++ sClass,
      screenZoomer = "boomer",
      screensaver = "xscreensaver-command -lock",
      screensaverBg = "xscreensaver -no-splash",
      screenshoter = "flameshot",
      setWallpaper = "~/scripts/set_wallpaper.sh",
      setupDualMonitors = "~/scripts/setup_multimonitors.sh",
      setupMonitor = "~/scripts/setup_monitor.sh",
      spotifyNext = spotifyDbus "Next",
      spotifyPrev = spotifyDbus "Previous",
      spotifyQt = "spotify-qt",
      spotifyToggle = spotifyDbus "PlayPause",
      suspend = "systemctl suspend",
      takeScreenshot = "flameshot gui",
      tray = "stalonetray --background '#222222' --geometry '4x1-0-0' --max-geometry '4x1' --window-strut bottom --icon-gravity ES --grow-gravity E --slot-size '33x33' --icon-size 24 --kludges=force_icons_size",
      udiskie = "udiskie",
      workspace =
        Workspace
          { dev = 0,
            web = 1,
            watch = 2,
            book = 3,
            play = 4,
            write = 5,
            bookmark = 6,
            music = 7,
            messenger = 8,
            experiment = 9
          },
      xcolor = "xcolor -s primary",
      xmobar = "~/.cabal/bin/myxmobar"
    }
  where
    sClass = "terminal.scratchpad"
    spotifyDbus event = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ event

colors :: Colors
colors =
  Colors
    { aqua = "#8ec07c",
      base00 = "#657b83",
      base01 = "#586e75",
      base02 = "#073642",
      base03 = "#002b36",
      bg = "#282828",
      bg1 = "#3c3836",
      bg2 = "#504945",
      bg3 = "#665c54",
      bg4 = "#7c6f64",
      blue = "#83a598",
      darkgreen = "#98971a",
      darkred = "#cc241d",
      fg = "#ebdbb2",
      gray = "#a89984",
      green = "#b8bb26",
      purple = "#d3869b",
      red = "#fb4934",
      white = "#ffffff",
      yellow = "#fabd2f"
    }

baseTheme :: Theme
baseTheme =
  def
    { activeColor = base03 colors,
      activeBorderColor = base03 colors,
      activeTextColor = base01 colors,
      inactiveBorderColor = base02 colors,
      inactiveColor = base02 colors,
      inactiveTextColor = base01 colors,
      urgentColor = yellow colors,
      urgentBorderColor = yellow colors,
      urgentTextColor = base02 colors,
      fontName = font' cfg,
      decoHeight = 20
    }

tabTheme :: Theme
tabTheme =
  baseTheme
    { activeColor = base00 colors,
      activeBorderColor = base03 colors,
      activeTextColor = white colors
    }

data Colors = Colors
  { aqua :: String,
    base00 :: String,
    base01 :: String,
    base02 :: String,
    base03 :: String,
    bg :: String,
    bg1 :: String,
    bg2 :: String,
    bg3 :: String,
    bg4 :: String,
    blue :: String,
    darkgreen :: String,
    darkred :: String,
    fg :: String,
    gray :: String,
    green :: String,
    purple :: String,
    red :: String,
    white :: String,
    yellow :: String
  }

data Config = Config
  { audioToggle :: String,
    bookmarkAdd :: String,
    bookmarkChoose :: String,
    brightnessDown :: String,
    brightnessUp :: String,
    browser :: String,
    clipboardManager :: String,
    compositor :: String,
    dmenu :: String,
    dmenuApp :: String,
    emojiYank :: String,
    font' :: String,
    gotoWindow :: String,
    kmonad :: String,
    kmonadKeycool :: String,
    liveWallpaperServer :: String,
    lockScreen :: String,
    lowerVolume :: String,
    terminal' :: String,
    passPrompt :: String,
    passPromptType :: String,
    raiseVolume :: String,
    rustLspMultiplex :: String,
    scratchpadClass :: String,
    scratchpadTerminal :: String,
    screenZoomer :: String,
    screensaver :: String,
    screensaverBg :: String,
    screenshoter :: String,
    setWallpaper :: String,
    setupDualMonitors :: String,
    setupMonitor :: String,
    spotifyNext :: String,
    spotifyPrev :: String,
    spotifyQt :: String,
    spotifyToggle :: String,
    suspend :: String,
    takeScreenshot :: String,
    tray :: String,
    udiskie :: String,
    workspace :: Workspace,
    xcolor :: String,
    xmobar :: String
  }

data Workspace = Workspace
  { dev :: Integer,
    web :: Integer,
    watch :: Integer,
    book :: Integer,
    play :: Integer,
    write :: Integer,
    bookmark :: Integer,
    music :: Integer,
    messenger :: Integer,
    experiment :: Integer
  }

workspaceIconM :: M.Map Integer String
workspaceIconM =
  M.fromList
    [ (dev wss, "\xf303"),
      (web wss, "\xfa9e"),
      (watch wss, "\xf947"),
      (book wss, "\xf02d"),
      (play wss, "\xf11b"),
      (write wss, "\xf8e9"),
      (bookmark wss, "\xf02e"),
      (music wss, "\xf7ca"),
      (messenger wss, "\xfa00"),
      (experiment wss, "\xe777")
    ]
  where
    wss = workspace cfg

ws :: (Workspace -> Integer) -> String
ws w = workspaceIconM ! w (workspace cfg)

wsIdxByIcon :: String -> Integer
wsIdxByIcon i =
  M.fromList (swap <$> M.toList workspaceIconM) ! i

extraWorkspaces :: [String]
extraWorkspaces = [ws experiment]

myWorkspaces :: [String]
myWorkspaces =
  fmap fromJust . takeWhile isJust $
    flip M.lookup workspaceIconM <$> [0 ..]

promptConf :: XPConfig
promptConf =
  def
    { font = font' cfg,
      autoComplete = Just 0,
      bgColor = "#000000",
      fgColor = "#f5ff6e",
      borderColor = "#2b2b2b",
      position = Top,
      height = 30
    }

data LayoutT = US | RU

instance Show LayoutT where
  show layout = case layout of
    US -> "us"
    RU -> "ru"

switchLayout :: LayoutT -> String
switchLayout = ("xkb-switch -s " ++) . show
