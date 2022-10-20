module Config (module Config) where

import XMonad.Layout.Decoration
import XMonad.Prompt

cfg :: Config
cfg =
    Config
        { audioToggle = "pactl set-sink-mute 0 toggle"
        , brightnessDown = "~/scripts/change_brightness.sh dec"
        , brightnessUp = "~/scripts/change_brightness.sh inc"
        , browser = "firefox"
        , clipboardManager = "parcellite"
        , compositor = "picom"
        , dmenu = "dmenu_run"
        , dmenuApp = "rofi -show combi -combi-modi 'window,run,ssh,drun' -modi combi -show-icons"
        , gotoWindow = "rofi -modi window -show window -show-icons"
        , lockScreen = "~/scripts/lockscreen.sh"
        , lowerVolume = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
        , myBar = "~/.config/polybar/launch.sh"
        , myFont = "xft:Iosevka Nerd Font Mono-14:style=term"
        , myTerminal = "alacritty"
        , passPrompt = "passmenu"
        , raiseVolume = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
        , scratchpadTerminalAdditionalOptions = " -o window.opacity=1.0 "
        , scratchpadTerminalTitle = "scratchpad_terminal"
        , screenZoomer = "boomer"
        , screensaver = "xscreensaver-command -lock"
        , screensaverBg = "xscreensaver -no-splash"
        , screenshoter = "flameshot"
        , setWallpaper = "~/scripts/set_wallpaper.sh"
        , setupDualMonitors = "~/scripts/setup_multimonitors.sh"
        , setupKeyboard = "~/scripts/setup_keyboard.sh"
        , setupMonitor = "~/scripts/setup_monitor.sh"
        , spotifyQt = "spotify-qt"
        , suspend = "systemctl suspend"
        , takeScreenshot = "flameshot gui"
        , udiskie = "udiskie"
        }

colors :: Colors
colors =
    Colors
        { aqua = "#8ec07c"
        , base00 = "#657b83"
        , base01 = "#586e75"
        , base02 = "#073642"
        , base03 = "#002b36"
        , bg = "#282828"
        , bg1 = "#3c3836"
        , bg2 = "#504945"
        , bg3 = "#665c54"
        , bg4 = "#7c6f64"
        , blue = "#83a598"
        , darkgreen = "#98971a"
        , darkred = "#cc241d"
        , fg = "#ebdbb2"
        , gray = "#a89984"
        , green = "#b8bb26"
        , purple = "#d3869b"
        , red = "#fb4934"
        , white = "#ffffff"
        , yellow = "#fabd2f"
        }

baseTheme :: Theme
baseTheme =
    def
        { activeColor = base03 colors
        , activeBorderColor = base03 colors
        , activeTextColor = base01 colors
        , inactiveBorderColor = base02 colors
        , inactiveColor = base02 colors
        , inactiveTextColor = base01 colors
        , urgentColor = yellow colors
        , urgentBorderColor = yellow colors
        , urgentTextColor = base02 colors
        , fontName = myFont cfg
        , decoHeight = 20
        }

tabTheme :: Theme
tabTheme =
    baseTheme
        { activeColor = base00 colors
        , activeBorderColor = base03 colors
        , activeTextColor = white colors
        }

data Colors = Colors
    { aqua :: String
    , base00 :: String
    , base01 :: String
    , base02 :: String
    , base03 :: String
    , bg :: String
    , bg1 :: String
    , bg2 :: String
    , bg3 :: String
    , bg4 :: String
    , blue :: String
    , darkgreen :: String
    , darkred :: String
    , fg :: String
    , gray :: String
    , green :: String
    , purple :: String
    , red :: String
    , white :: String
    , yellow :: String
    }

data Config = Config
    { audioToggle :: String
    , brightnessDown :: String
    , brightnessUp :: String
    , browser :: String
    , clipboardManager :: String
    , compositor :: String
    , dmenu :: String
    , dmenuApp :: String
    , gotoWindow :: String
    , lockScreen :: String
    , lowerVolume :: String
    , myBar :: String
    , myFont :: String
    , myTerminal :: String
    , passPrompt :: String
    , raiseVolume :: String
    , screenZoomer :: String
    , screenshoter :: String
    , screensaverBg :: String
    , screensaver :: String
    , setWallpaper :: String
    , setupKeyboard :: String
    , setupMonitor :: String
    , setupDualMonitors :: String
    , spotifyQt :: String
    , suspend :: String
    , takeScreenshot :: String
    , udiskie :: String
    , scratchpadTerminalTitle :: String
    , scratchpadTerminalAdditionalOptions :: String
    }

promptConf :: XPConfig
promptConf =
    def
        { font = myFont cfg
        , autoComplete = Just 0
        , bgColor = "#000000"
        , fgColor = "#f5ff6e"
        , borderColor = "#2b2b2b"
        , position = Top
        , height = 30
        }

