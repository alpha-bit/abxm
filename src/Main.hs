--------------------------------------------------------------------------------
-- | Module    :  Main
--
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--------------------------------------------------------------------------------
-- This is the default Main entry point for the
-- ABC window manager based on XMmonad.
--
-- Take a look at 'ABXMDefaults' source code
-- and learn how to configure it.
--
module Main where
--------------------------------------------------------------------------------
--
--
import XMonad
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
--
import XMonad.Layout.Monitor (MonitorMessage(..))
import XMonad.Prompt
--import XMonad.Prompt.Shell ()
--
import XMonad.Util.WindowProperties (Property(ClassName, Title, Or))
--
import XMonad.Config.ABC
import XMonad.Config.ABCDefaults
import XMonad.Prompt.Terminator
--
-- import XMonad.Actions.Commands -- idea: prompt for actions / show help
-- import XMonad.Actions.CycleSelectedLayouts -- idea: up>cycleFull down>cycleTallMirror
-- import XMonad.Actions.FloatSnap -- mouse snap
-- XMonad.Actions.GridSelect workSpace, run, actions
-- XMonad.Actions.KeyRemap ... parentisis
-- XMonad.Actions.RandomBackground -- for terminator bluish, redish, greeny
-- XMonad.Actions.Search -- internet search
-- XMonad.Hooks.FloatNext -- float mode on/off
-- XMonad.Hooks.GroupNavigation -- for main root terminal in drawer
-- sink to drawer/destop
-- s: system tray... i: choose icon tray app to toggle
main = abxmMain gnomeConfig abXMonad
  { keyboard = abKeyboard
    { kExtra = abExtraKeys
      { ekActions =
        [(controlMask,
          [ (xK_m, abManPrompt $ ppPrompt abPreatyPrint)
          --, (xK_a, broadcastMessage ToggleMonitor >> refresh)
          --, (xK_p, defaultCommands >>= runCommand)
          ]-- sTDT ++ " -x xprop")]
        )]
      , ekSpawns =
        [ (0,
          [ (xK_KP_Add, "amixer set Master 5%+")
          , (xK_KP_Subtract, "amixer set Master 5%-")
          --
          , (0x1008FF11, "amixer set Master 5%-")
          , (0x1008FF12, "amixer set Master toggle")
          , (0x1008FF13, "amixer set Master 5%+")
          ])
        , (controlMask,
          [ (xK_a, "killall qasmixer || qasmixer -t")
          --
          , (xK_b, "killall clipit || clipit")
          --
          , (xK_v, "killall cameramonitor || cameramonitor") -- monitor Video
          --
          , (xK_o, "killall vino || vino") -- Open desktop
          --
          , (xK_n, "killall nm-applet || nm-applet") -- Network manager
          --
          -- ADD TO DEFAULTS
          --
          , (xK_d, "kdocker -f") -- Dock
          --
          , (xK_l, "luakit ~/.xmonad/abxm/doc/frames.html") -- view in Luakit
          --
          , (xK_x, "xprop | xmessage -file -")
          , (xK_g, "killall xcompmgr || xcompmgr -cf") -- graphic efects
          --
          , (xK_t, "killall trayer || " ++ abxmB ++ "trayer") -- trayer
          , (xK_s, "killall " ++ sXS ++ "|| " ++ abxmB ++ sXS ++ "laptop") -- DESKTOP/LAPTOP
          ]
        )]
      }
    }
  , managerHooks = abManagerHooks
    { mhPerWindow = mhPerWindow abManagerHooks ++
      [ (doCenterFloat,
        [ (title,
          [ "ALSA Mixer", "System Monitor"
          , "Downloads", "Eclipse", "Iceweasel Preferences", "Save As..."
          , "Guake Preferences"
          , "QuickProxy Preferences"
          , "Dictionary"
          , "Kupfer Preferences"
          , "Terminator Preferences"
          ])
        ])
      , (doFloat,
        [ (title, ["Budy List"])
        , (className, ["Amdcccle", "Kcalc", "qjackctl.real", "MPlayer", "Sysinfo", "XCalc"])
        , (className, ["Gnote", "feh", "luvcview", "Sonata"])
        ])
      , (doIgnore,
        [ (resource, ["trayer"])
        ])
      ]
    , mhPerWorkSpace =
      [ (doShift,
        [ ("i", [ (className, [])])
        ]
      )]
    }
  , workSpace = abWS
    { wsIsTagShiftable = (\t -> wsIsTagShiftable abWS t && (t /= lastTag))
    -- wsIsTagA = (\t -> wsIsTagShiftable abWS t && (t /= lastTag))
    -- wsIsTagB = (\t -> wsIsTagShiftable abWS t && (t /= lastTag))
    -- wsIsTagC = (\t -> wsIsTagShiftable abWS t && (t /= lastTag))
    --
    , wsKeys = wsKeys abWS ++ [xK_KP_Delete]
    , wsTags = wsTags abWS ++ [lastTag]
    }
  , popDrawer = abPopDrawer
    { tdWindows = tdWindows abPopDrawer
      `Or` Title "Guake!"
      --`Or` ClassName "Sonata"
      `Or` Title "System Monitor"
    }
  }
  where
    abxmB = abxmHomePathSlash ++ "bin/abxm-"
    sXS = "system-xmobar " --systemXmobarSpace
    --
    abWS = abWorkSpace
    lastTag = "."
    sTDT = sTopDrawerTerminal abSpawns
