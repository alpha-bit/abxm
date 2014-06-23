---------------------------------------------------------------------------------
-- | Module    :  XMonad.Config.ABCDefaults
--
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--------------------------------------------------------------------------------
-- Provides a default instance for 'ABC'.
--
module XMonad.Config.ABCDefaults where
--------------------------------------------------------------------------------
--
import XMonad.Config.ABC
import XMonad.Actions.FloatMatrix (defaultFloatMatrix, FloatMatrix (..))
--
import Control.Monad (liftM2)
--
import Data.List  (intercalate)
import Data.Map (Map, fromList)
import Data.Maybe (fromJust, isJust, isNothing)
--
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO   (hPutStrLn)
--
import XMonad
--
import XMonad.Actions.CopyWindow
  (copy, copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS
  ( WSType(EmptyWS, NonEmptyWS, WSIs), Direction1D( Prev, Next)
  , moveTo, prevWS, nextWS, shiftToPrev, shiftToNext, toggleOrDoSkip
  )
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
  {-, withWorkspace, selectWorkspace, renameWorkspace) -}
import XMonad.Actions.NoBorders
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers
  ( (-?>)
  , doCenterFloat, doFullFloat, doSideFloat
  , isDialog, isFullscreen, isKDETrayWindow
  , transience'
  , MaybeManageHook
  , Side (NW, NE, CW, CE, SE, SW)
  )
import XMonad.Hooks.SetWMName
import XMonad.Layout
  ( ChangeLayout (FirstLayout, NextLayout)
  , Resize       (Expand, Shrink)
  )
import XMonad.Layout.Drawer
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.WindowProperties

--import XMonad.Layout.NoBorders
--import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize

import XMonad.Layout.LayoutModifier
import XMonad.Operations (float)
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Shell      (shellPrompt)
import XMonad.Prompt.Window     (windowPromptGoto)
--
import XMonad.Util.Run      (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
--
import qualified Data.Map as D_M
import qualified XMonad.StackSet as WS
--
--
-- abcFont size
abcFont s = "xft:Droid Sans Mono:size=" ++ (show s) ++ "px:antialias=true:weight=bold:style:italic"
  --"-*-Fixed-Bold-*-*-*-" ++ (show s) ++ "-*-*-*-*-*-*-*"
--
-------------------------------------------------------------------------------
-- configure above this line
-------------------------------------------------------------------------------
--
abxmMain xC abXM = do
  --
  lsph <- -- log spawn pipe handle
    spawnPipe $ sLogPipe $ spawns abXM
  --
  -- start xmonad
  xmonad $ abXMConfig xC abXM lsph
--
---------------------
-- default instances
--
abXMonad = ABXMonad
  { favoriteSpawns    = abFavoriteSpawns
  , floatMatrix       = abFloatMatrix
  , keyboard          = abKeyboard
  , managerHooks      = abManagerHooks
  , mediaPlayerSpawns = abMediaPlayerSpawns
  , mouse             = abMouse
  , popDrawer         = abPopDrawer
  , preatyPrint       = abPreatyPrint
  , spawns            = abSpawns
  , window            = abWindow
  , workSpace         = abWorkSpace
  }
--
--
abFavoriteSpawns = ABFavoriteSpawns
  { fsRoot         = fsUser abFS ++ ["synaptic"]
  , fsRootTerminal = fsUserTerminal abFS ++ []
  , fsUser         = ["terminator"]
  , fsUserTerminal = ["mc"]
  , fsXsuPrefix    = "gksu"
  } where abFS = abFavoriteSpawns
--
--
abFloatMatrix =
  (defaultFloatMatrix $ 0:map (\f -> f abModMasks) [mmAlt, mmHigh, mmLow])
    { gapWidth = 1 }
--
--
abKeyboard = ABKeyboard
  { kExtra       = abExtraKeys
  , kMasks       = abModMasks
  , kMulti       = abMultiKeys
  , kMusicPlayer = abMusicPlayerKeys
  , kXMonad      = abXMonadKeys
  --
  , kScreen = [xK_Page_Up, xK_Page_Down]
  }
--
abExtraKeys = ABExtraKeys
  { ekActions = []
  , ekSpawns  = []
  }
--
abModMasks = ABModMasks
  { mmCommon = mod4Mask
  , mmAlt  = mod1Mask
  , mmHigh = shiftMask
  , mmLow  = controlMask
  }
--
abMultiKeys = ABMultiKeys
  { blankHMonitorLLock             = xK_Pause
  , borderHStruts                  = xK_KP_Divide
  , closeHColapseLKill             = xK_BackSpace
  , masterHShiftLReset             = xK_Home
  , nextHShiftLNext                = xK_Down
  , nonAEmptyHShiftLNextAl         = xK_Right
  , nonAEmptyHShiftLPreviousAl     = xK_Left
  , previousHShiftLFirst           = xK_Up
  , raiseHRunLJust                 = xK_Tab
  , runATerminalHRootAhTerminal    = xK_Return
  , terminalHDrawerLSinked         = xK_Escape
  , transparencyHIncreaseLDecrease = xK_KP_Multiply
  , unfloatHExpandLShrink          = xK_Insert
  , windowHScreenshotLWallpapper   = xK_Print
  }
--
abMusicPlayerKeys = ABMusicPlayerKeys
  { mpkMask         = mmAlt abMM
  , mpkF_wMask      = mmLow abMM
  --
  , mpkMoveToTrash  = xK_KP_Delete
  --
  , mpkToglePlay    = blankHMonitorLLock abMultiKeys
  , mpkVolumeUp     = xK_KP_Add
  , mpkVolumeDown   = xK_KP_Subtract
  --
  , mpmkNextFFw     = xK_Page_Down
  , mpmkPreviousFBw = xK_Page_Up
  --
  } where abMM = abModMasks
--
abXMonadKeys = ABXMonadKeys
  { xmkMask = mmAlt abMM .|. mmLow abMM
  --
  , xmkCompile = xK_c
  , xmkQuit    = xK_q
  , xmkRestart = xK_r
  --
  } where abMM = abModMasks
--
--
abManagerHooks = ABManagerHooks
  { mhActions =
    [ isDialog --> doCenterFloat
    , isFullscreen --> doFullFloat
    , isKDETrayWindow --> doIgnore
    , transience'
    ]
  , mhPerWindow =
    [ (doCenterFloat, [(className, ["Xmessage"])])
    , (doIgnore, [(resource,
      [ "desktop", "desktop_window", "notify-osd", "stalonetray", "trayer"])])
    ]
  , mhPerWorkSpace = []
  }
--
--
abMediaPlayerSpawns = ABMediaPlayerSpawns
  { sMPTogglePlay   = "mpc toggle"
  --
  , sMPNext         = "mpc next"
  , sMPPrevious     = "mpc prev"
  --
  , sMPFastBackward = "mpc seek -0:0:1"
  , sMPFastForward  = "mpc seek +0:0:1"
  --
  , sMPVolumeDown   = "mpc volume -7"
  , sMPVolumeUp     = "mpc volume +7"
  --
  , sMPTrashCurrent = abxmHomePathSlash
    ++ "bin/ab-mpd-trash | xmessage -file -"
  }
--
--
abMouse = ABMouse
  { mBindings =
    -- left   : move window
    [ (0,
      [ (button1, (\w -> focus w >> mouseMoveWindow w))
      -- middle : move to master or raise if floating
      , (button2, (\w -> focus w >> windows WS.shiftMaster))
      -- right  : resize window
      , (button3, (\w -> focus w >> mouseResizeWindow w >> windows WS.shiftMaster))
      -- wheel  : change workspace
      --, (button4, (\w -> focus w >> moveTo Prev $ WSIs nonEmptyAndShitable))
      --, (button5, (\w -> focus w >> moveTo Next $ WSIs nonEmptyAndShitable))
      ])
    ]
  --
  , mCariesFocus       = False
  , mClickJustFocuses  = False
  }
--
--
abPopDrawer = ABPopDrawer
  { tdClosedSize = 0.002
  , tdOpenSize   = 0.35
  --
  , tdWindows    = Role abCRDrawer `Or` ClassName abCRDrawer
  }
--
--
abPreatyPrint = ABPreatyPrint
  { ppLogPipe = xmobarPP
    { ppCurrent = xmc "gold"
    , ppVisible = xmc "goldenrod"
    , ppHidden  = xmc "gray"
    , ppSep     = xmc "skyblue" "." ++ xmc "gold" "/ "
    , ppWsSep   = xmc "skyblue" "."
    , ppUrgent  = xmc "orangered"
    , ppTitle   = xmc "white"
    , ppOrder   = \(w:_:t:_) -> ["", w, t]
    }
  , ppPrompt = defaultXPConfig
    { autoComplete = Just 1
    , bgColor      = "dimgray"
    , bgHLight     = "dimgray"
    , borderColor  = "black"
    , fgHLight     = "limegreen"
    , font         = abcFont 10
    , height       = 28
    , position     = Top
    }
  }
  where xmc c = xmobarColor c ""
--
--
abSpawns = ABSpawns
  { sLogPipe = "exec xmobar " ++ hps ++ "etc/abxm.xmobar"
  --
  , sStartup = sReloadWallpaper abSpawns ++ ';'
    : abBuildAutostartSpawn "xmessage \"$text\";" -- show it...
  --
  , sCompileXmonad   = abxmCompileSpawn ++ " 2>&1 | xmessage -file -"
  --
  , sConfigureXmonad = sTDT ++ " -x mcedit ~/.xmonad/abxm/src/Main.hs"
  --
  , sDecreaseTransparency = sTS ++ " --dec .1"
  , sIncreaseTransparency = sTS ++ " --inc .1"
  , sToggleTransparency   = sTS ++ "t .21"
  --
  , sReloadWallpaper = "feh --bg-max --no-fehbg -rFz "++ hps
    ++ "etc/wallpapers"
  --
  , sSystemMonitor = sTDT ++ " -x top"
  --
  , sWindowShot = "scrot -u " ++ sps ++ "windowshot.png"
  , sScreenShot = "scrot " ++ sps ++ "sceenshot.png"
  --
  , sTopDrawerTerminal = "terminator -p " ++ abCRDrawer ++ " -r " ++ abCRDrawer
  , sTerminal          = "terminator"
  --
  , sXBlank = "sleep 1 && xset s activate"
  , sXKill  = "xkill"
  , sXLock  = "xtrlock"
  } where
    hps = abxmHomePathSlash
    sps = "~/tmp/"
    sTDT = sTopDrawerTerminal abSpawns
    sTS  = "transset -a"
--
--
abWindow = ABWindow
  { wBorderFocusedColor = "gold"
  , wBorderNormalColor  = "dimgray"
  --
  , wBorderWidth  = 2
  , wGapWidth     = 1
  --
  , wInactiveOpacity = 0.84
  }
--
--
abWorkSpace = ABWorkSpace
  { wsIsTagShiftable = (\t -> notElem (t !! 0) ['0'..'9'])
  --
  , wsKeys = [xK_0..xK_9]
  , wsTags = map (:[]) ['0'..'9']
  --
  , wsShiftableKeys = [xK_a..xK_z]
  , wsShiftableTags = map (:[]) ['a'..'z']
  --
  }
--
