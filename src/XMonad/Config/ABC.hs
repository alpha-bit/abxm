--------------------------------------------------------------------------------
-- | Module    :  XMonad.Config.ABC
--
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--------------------------------------------------------------------------------
-- Declares ABMX data types and provides functions to handle them.
--
module XMonad.Config.ABC where
--------------------------------------------------------------------------------
--
import XMonad.Actions.FloatMatrix
  (flatBindingsTree, flatBindingsTreeToFocused, matrixBindings, FloatMatrix)
--
import Control.Monad (liftM2)
--
--import Data.List  (intercalate)
import Data.Map   (fromList)
import Data.Maybe (isJust, isNothing)
--
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO   (hPutStrLn)
--
--
import XMonad hiding (floatLocation, float)
--
--import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
--
import XMonad.Actions.CopyWindow (copy, kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS
  ( WSType(EmptyWS, NonEmptyWS, WSIs), Direction1D( Prev, Next)
  , moveTo, prevWS, nextWS, shiftToPrev, shiftToNext, toggleOrDoSkip
  )
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.NoBorders         (toggleBorder)
--
import XMonad.Hooks.DynamicLog   (dynamicLogWithPP, PP (ppOutput))
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
  ( avoidStruts, docksEventHook, manageDocks
  , ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers
  ( (-?>)
  , doCenterFloat, doFullFloat, doSideFloat, doFloatDep
  , isDialog, isFullscreen, isKDETrayWindow
  , transience'
  , MaybeManageHook
  , Side (C, CE, CW, NC, NE, NW, SC, SE, SW)
  )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.Place (placeFocused, fixed)
--
import XMonad.Layout
  ( ChangeLayout (FirstLayout, NextLayout)
  , Resize       (Expand, Shrink)
  )
import XMonad.Layout.Drawer   (simpleDrawer, onBottom)
import XMonad.Layout.Spacing  (spacing)
--import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutModifier (ModifiedLayout(ModifiedLayout))
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.MouseResizableTile
  (draggerType, isMirrored, mouseResizableTile, DraggerType(BordersDragger))
--
--import XMonad.Operations (float)
--
import XMonad.Prompt            (XPConfig)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Shell      (shellPrompt)
import XMonad.Prompt.Window     (windowPromptGoto)
--
--import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WindowProperties (Property(Role))
--
import qualified Data.Map as D_M
import qualified Data.Set as D_S
import qualified XMonad.StackSet as W
--
--
--
abcMHMoves = -- [ ("a", []), ("b", []), ("c", [])
  -- alphabit
  [ ("0", ["Synaptic"])
  , ("7", ["Sonata", "xine", "MPlayer"])
  , ("8", ["Gnote"])
  , ("a", [])
  , ("b", ["Blender"])
  -- c, c++
  , ("c", [])
  , ("e", ["Eclipse"])
  , ("f", ["Dolphin", "Nautilus"]) -- ghci
  , ("g", ["Gimp", "Inkscape"])
  , ("i", ["Iceweasel", "Chromium"])
  , ("l", ["Leksah"])
  , ("v", ["VirtualBox"]) --
  ]
--
abcMHShifts = []
--
-------------------------------------------------------------------------------
-- configure above this line
-------------------------------------------------------------------------------
--
-- | the home directory of ABXM module
abxmHomePathSlash :: String -- ^ "~/.xmonad/ABXM/"
abxmHomePathSlash = "~/.xmonad/abxm/"
--
-- | builds a bash script that runs 'abxmHomeDirectory'/etc/autostart
-- if any output is stream the given bash script is executed.
-- the script may have access to the outputted through the environment variable $text.
abBuildAutostartSpawn ::
  String -- ^ the bash script to execute
  -> String -- ^ the given command surrounded by the autostart script.
abBuildAutostartSpawn bs =
  "abA=" ++ abxmHomePathSlash ++ "etc/autostart;" -- abxmAutostart
  ++ "if [ -x \"$abA\" ]; then " -- is executable?
  ++     "text=\"`$abA`\";" -- abAResult = the output of its execution
  ++     "if [ \"$text\" ]; then " -- returned something?
  ++       bs -- execute given script
  ++ "fi;fi;"
--
--
abxmCompileSpawn = "~/.xmonad/abxm/bin/abxm-compile"
--
--
abDoTallFloat h s = doFloatDep move where
  move (W.RationalRect _ _ w _) = W.RationalRect x y w h where
    x = if s `elem` [SC,C ,NC]
            then (1 - w) / 2
            else if s `elem` [SW,CW,NW]
            then 0 -- + m
            else 1 - w -- m -- s `elem` [SE,CE,NE]
    y = if s `elem` [CE,C ,CW]
            then (1 - h) / 2
            else if s `elem` [NE,NC,NW]
            then 0 -- + m
            else 1 - h -- m -- s `elem` [SE,SC,SW]
--
{-
import XMonad.Layout.Monitor (manageMonitor, monitor, Monitor(..))
abxmMonitor = monitor {
    -- Cairo-clock creates 2 windows with the same classname, thus also using title
    prop = Role abCRMonitor -- `And` Title "MacSlow's Cairo-Clock"
    -- rectangle 150x150 in lower right corner, assuming 1280x800 resolution
  , rect = Rectangle 25 (28 + 25) 770 350
    -- avoid flickering
  , persistent = True
    -- make the window transparent
  , opacity = 0
    -- hide on start
  , visible = False
    -- assign it a name to be able to toggle it independently of others
  , name = abCRMonitor
  }

 $ ModifiedLayout abxmMonitor -- does not avoid struts :(

 manageMonitor abxmMonitor

"touch /tmp/abxm-monitor && "
++ "terminator -br abxm-monitor -p abxm-monitor -x"
++ " tail -f /tmp/abxm-monitor && "
  -}
-------------------------------------------------------------------------------
-- | helper functions
--
-- | the class name or role to use in drawer windows
abCRDrawer :: String -- ^ "abxm-drawer"
abCRDrawer  = "abxm-drawer"
--
-- ^ the class name or role to use in monitor window
-- abCRMonitor :: String -- ^ "abxm-monitor"
-- abCRMonitor  = "abxm-monitor"
--
-- | the class name or role to use in floating windows
abCRFloat :: String -- ^ "abxm-float"
abCRFloat  = "abxm-float"
--
-- | the class name or role to use in floating windows
abCRFullFloat :: String -- ^ "abxm-full-float"
abCRFullFloat       = "abxm-full-float"
--
abCRFloatCC :: String -- ^ "abxm-float-cc"
abCRFloatCC = "abxm-float-cc"
--
abCRFloatCE :: String -- ^ "abxm-float-ce"
abCRFloatCE = "abxm-float-ce"
--
abCRFloatCW :: String -- ^ "abxm-float-cw"
abCRFloatCW = "abxm-float-cw"
--
abCRTallFloatCC :: String -- ^ "abxm-float-cc"
abCRTallFloatCC = "abxm-tall-float-cc"
--
abCRTallFloatCE :: String -- ^ "abxm-float-ce"
abCRTallFloatCE = "abxm-tall-float-ce"
--
abCRTallFloatCW :: String -- ^ "abxm-float-cw"
abCRTallFloatCW = "abxm-tall-float-cw"
{-
abCRFloatNC :: String -- ^ "abxm-float-nc"
abCRFloatNC = "abxm-float-nc"
--
abCRFloatNE :: String -- ^ "abxm-float-ne"
abCRFloatNE = "abxm-float-ne"
--
abCRFloatNW :: String -- ^ "abxm-float-nw"
abCRFloatNW = "abxm-float-nw"
--
abCRFloatSC :: String -- ^ "abxm-float-sc"
abCRFloatSC = "abxm-float-sc"
--
abCRFloatSE :: String -- ^ "abxm-float-se"
abCRFloatSE = "abxm-float-se"
--
abCRFloatSW :: String -- ^ "abxm-float-sw"
abCRFloatSW = "abxm-float-sw"
-}
-- | the class name or role to use in ignored windows
abCRIgnore :: String -- ^ "abxm-ignore"
abCRIgnore  = "abxm-ignore"
--
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"
--
-------------------------------------------------------------------------------
--
data ABXMonad = ABXMonad
  { favoriteSpawns    :: ABFavoriteSpawns
  , floatMatrix       :: FloatMatrix
  , keyboard          :: ABKeyboard
  , managerHooks      :: ABManagerHooks
  , mediaPlayerSpawns :: ABMediaPlayerSpawns
  , mouse             :: ABMouse
  , popDrawer         :: ABPopDrawer
  , preatyPrint       :: ABPreatyPrint
  , spawns            :: ABSpawns
  , window            :: ABWindow
  , workSpace         :: ABWorkSpace
  }
--
--
data ABFavoriteSpawns = ABFavoriteSpawns
  { fsRoot         :: [String]
  , fsRootTerminal :: [String]
  , fsUser         :: [String]
  , fsUserTerminal :: [String]
  , fsXsuPrefix    :: String
  }
--
--
data ABKeyboard = ABKeyboard
  { kExtra       :: ABExtraKeys
  , kMasks       :: ABModMasks
  , kMulti       :: ABMultiKeys
  , kMusicPlayer :: ABMusicPlayerKeys
  , kXMonad      :: ABXMonadKeys
  --
  , kScreen :: [KeySym]
  }
--
data ABExtraKeys = ABExtraKeys
  { ekActions :: [(KeyMask, [(KeySym, X ())])]
  , ekSpawns  :: [(KeyMask, [(KeySym, String)])]
  }
--
data ABModMasks = ABModMasks
  { mmCommon :: KeyMask
  --
  , mmAlt  :: KeyMask
  , mmHigh :: KeyMask
  , mmLow  :: KeyMask
  }
--
data ABMultiKeys = ABMultiKeys
  { blankHMonitorLLock             :: KeySym
  , borderHStruts                  :: KeySym
  , closeHColapseLKill             :: KeySym
  , masterHShiftLReset             :: KeySym
  , nextHShiftLNext                :: KeySym
  , nonAEmptyHShiftLNextAl         :: KeySym
  , nonAEmptyHShiftLPreviousAl     :: KeySym
  , previousHShiftLFirst           :: KeySym
  , raiseHRunLJust                 :: KeySym
  , runATerminalHRootAhTerminal    :: KeySym
  , terminalHDrawerLSinked         :: KeySym
  , transparencyHIncreaseLDecrease :: KeySym
  , unfloatHExpandLShrink          :: KeySym
  , windowHScreenshotLWallpapper   :: KeySym
  }
--
data ABMusicPlayerKeys = ABMusicPlayerKeys
  { mpkMask         :: KeyMask
  , mpkF_wMask      :: KeyMask
  --
  , mpkMoveToTrash  :: KeySym
  --
  , mpkToglePlay    :: KeySym
  , mpkVolumeUp     :: KeySym
  , mpkVolumeDown   :: KeySym
  --
  , mpmkNextFFw     :: KeySym
  , mpmkPreviousFBw :: KeySym
  }
--
data ABXMonadKeys = ABXMonadKeys
  { xmkMask :: KeyMask
  --
  , xmkCompile :: KeySym
  , xmkQuit    :: KeySym
  , xmkRestart :: KeySym
  }
--
--
data ABManagerHooks = ABManagerHooks
  { mhActions      :: [ManageHook]
  , mhPerWindow    :: [(ManageHook, [(Query String, [String])])]
  , mhPerWorkSpace ::
    [ (WorkspaceId -> ManageHook
    , [(String, [(Query String, [String])])])
    ]
  }
--
--
data ABMediaPlayerSpawns = ABMediaPlayerSpawns
  { sMPTogglePlay   :: String
  --
  , sMPNext         :: String
  , sMPPrevious     :: String
  --
  , sMPFastBackward :: String
  , sMPFastForward  :: String
  --
  , sMPVolumeDown   :: String
  , sMPVolumeUp     :: String
  --
  , sMPTrashCurrent :: String
  }
--
--
data ABMouse = ABMouse
  { mBindings          :: [(KeyMask, [(Button, (Window -> X ()))])]
  --
  , mCariesFocus       :: Bool
  , mClickJustFocuses  :: Bool
  }
--
--
data ABPopDrawer = ABPopDrawer
  { tdOpenSize     :: Rational
  , tdClosedSize   :: Rational
  --
  , tdWindows      :: Property
  }
--
--
data ABPreatyPrint = ABPreatyPrint
  { ppLogPipe :: PP
  , ppPrompt  :: XPConfig
  }
--
--
data ABSpawns = ABSpawns
  { sLogPipe              :: String
  , sStartup              :: String
  --
  , sCompileXmonad        :: String
  , sConfigureXmonad      :: String
  --
  , sDecreaseTransparency :: String
  , sIncreaseTransparency :: String
  , sToggleTransparency   :: String
  --
  , sReloadWallpaper      :: String
  --
  , sSystemMonitor        :: String
  --
  , sScreenShot           :: String
  , sWindowShot           :: String
  --
  , sTopDrawerTerminal    :: String
  , sTerminal             :: String
  --
  , sXBlank               :: String
  , sXKill                :: String
  , sXLock                :: String

  }
--
--
data ABWindow = ABWindow
  { wBorderFocusedColor :: String
  , wBorderNormalColor  :: String
  --
  , wBorderWidth  :: Dimension
  , wGapWidth     :: Int
  --
  , wInactiveOpacity :: Rational
  }
--
--
data ABWorkSpace = ABWorkSpace
  { wsIsTagShiftable :: String -> Bool
  --
  , wsKeys :: [KeySym]
  , wsTags :: [String]
  --
  , wsShiftableKeys :: [KeySym]
  , wsShiftableTags :: [String]
  }
--
--
-- parentXConfig abXMonad logSpawPipe
abXMConfig pxc abxm lsph =  pxc -- was: def
  --
  -- terminal
  { terminal = sTerminal abS
  --
  -- border
  , borderWidth        = wBW
  , focusedBorderColor = wBorderFocusedColor abW
  , normalBorderColor  = wBorderNormalColor abW
  --
  -- workspaces
  , workspaces = wsShiftableTags abWS ++ wsTags abWS
  --
  -- keyboard
  , keys    = assembleKeys
  --
  , modMask = mmC
  --
  -- mouse
  -- , clickJustFocuses  = mClickJustFocuses abM
  , focusFollowsMouse = mCariesFocus abM
  --
  , mouseBindings     = (\_->fromList $ flatBindingsTree mmC $ mBindings abM)
  --
  --
  , handleEventHook = docksEventHook
  --
  -- layout hook
  , layoutHook =
    spacing wGW
    $ maximize
    $ (simpleDrawer (tdClosedSize abTD) (tdOpenSize abTD)
      (tdWindows abTD) `onBottom`) -- out of struts
    $ avoidStruts
    $ mouseResizableTile {isMirrored = True, draggerType = BordersDragger}
      ||| mouseResizableTile {draggerType = BordersDragger}
      ||| Full
  --
  -- log hook
  , logHook = dynamicLogWithPP (ppLogPipe abPP) {ppOutput = hPutStrLn lsph}
  --
  -- manageHook
  , manageHook = manageDocks
    <+> (composeAll . concat $
      [ [isDialog --> doCenterFloat]
      , mhActions abMH
      , concatMap (ftxll) $ mhPerWindow abMH
      , concatMap (ftxll) $ windowHooks
      ] ++ map  (s doShift) abcMHShifts
      ++ map (s (doF . liftM2 (.) W.greedyView W.shift)) abcMHMoves)
  --
  -- startup
  , startupHook = setWMName "LG3D" -- fix java bugs they say
    <+> (spawn $ sStartup abS)
  }
  where
    mmC = mmCommon abKM
    --
    ftxll (f, txll) = concatMap (\(t, xl) -> [(t =? x) --> f | x <- xl]) txll
    --
    d f sl = -- do function sourceList
      [(title =? x <||> className =? x <||> resource =? x) --> f | x <- sl]
    -- shift shiftFuncion (workspaceId, workSpaceWindowList)
    s sf (wi, wswl) = d (sf wi) wswl
    --
    windowHooks =
      [ (doFloat    , [(className, [abCRFloat])    , (role, [abCRFloat])])
      , (doFullFloat, [(className, [abCRFullFloat]), (role, [abCRFullFloat])])
      --
      , (doSideFloat C , [(className, [abCRFloatCC]), (role, [abCRFloatCC])])
      , (doSideFloat CE, [(className, [abCRFloatCE]), (role, [abCRFloatCE])])
      , (doSideFloat CW, [(className, [abCRFloatCW]), (role, [abCRFloatCW])])
      --
      , (abDTF C , [(className, [abCRTallFloatCC]), (role, [abCRTallFloatCC])])
      , (abDTF CE, [(className, [abCRTallFloatCE]), (role, [abCRTallFloatCE])])
      , (abDTF CW, [(className, [abCRTallFloatCW]), (role, [abCRTallFloatCW])])
{-
      , (doSideFloat NC, [(className, [abCRFloatNC]), (role, [abCRFloatNC])])
      , (doSideFloat NE, [(className, [abCRFloatNE]), (role, [abCRFloatNE])])
      , (doSideFloat NW, [(className, [abCRFloatNW]), (role, [abCRFloatNW])])
      , (doSideFloat SC, [(className, [abCRFloatSC]), (role, [abCRFloatSC])])
      , (doSideFloat SE, [(className, [abCRFloatSE]), (role, [abCRFloatSE])])
      , (doSideFloat SW, [(className, [abCRFloatSW]), (role, [abCRFloatSW])])
-}
      --
      , (doIgnore, [(className, [abCRIgnore]), (role, [abCRIgnore])])
      ]
    --
    -- abSideFloat margin side
    abDTF = abDoTallFloat 0.9
    --
    -- abxm shorts
    abFA  = floatMatrix    abxm
    abM   = mouse        abxm
    abMH  = managerHooks abxm
    abPP  = preatyPrint  abxm
    abS   = spawns       abxm
    abTD  = popDrawer    abxm
    abW   = window       abxm
    abWS  = workSpace    abxm
    --
    abK   = keyboard    abxm
    abEK  = kExtra       abK
    abKM  = kMasks       abK
    abMK  = kMulti       abK
    abMPK = kMusicPlayer abK
    abXMK = kXMonad      abK
    --
    wBW = wBorderWidth abW
    wGW = wGapWidth abW
    --
    -- assemble keys
    assembleKeys conf@(XConfig {XMonad.modMask = mmC}) = D_M.fromList $
      -- switch to physical/Xinerama screen
      -- shift : move window to screen
      -- control : switch workspace with other screen
      [ ((m .|. mmC, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip (kScreen abK) [0..]
      , (f, m) <- [(W.view, 0), (W.shift, mmH), (W.greedyView, mmL)]
      ]
      -- non spawn keys, aka: inner keys
      ++ (concatMap (\(mm, knsl) -> map (nsmf mm) knsl) $ multiKeys ++ ekActions abEK)
      -- spawn keys
      ++ (concatMap (\(mm, kscl) -> map (\(k, sf) ->
        smf mm (k, sf abS)) kscl) $ spawnMultiKeys)
      -- media player spawn keys
      ++ (concatMap (\(mm, kscl) -> map (\(k, sf) ->
        smf mm (k, sf $ mediaPlayerSpawns abxm)) kscl) $ mpdSpawnKeys)
      -- spawn keys
      ++ (concatMap (\(mm, kscl) -> map (smf mm) kscl) $ ekSpawns abEK)
      -- float area keys
      ++ (flatBindingsTreeToFocused mmC $ matrixBindings abFA wmf00 wmf00)
      -- go/shift/copy to workspace k
      ++ [ ((mmC .|. m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) wsAllKeys
        , (f, m) <- [(W.view, 0), (W.shift, mmH), (copy, mmA)]
        ]
        --
      where
        wmf00 = sendMessage.maximizeRestore -- temporary fullsceen float
        -- nonSpawnMapFunction mask key nonSpawn
        nsmf mm (k, ns) = ((mm .|. mmC, k), ns)
        -- spanMapFunction mask key spawnCommand
        smf mm (k, sc) = nsmf mm (k, spawn $ sc)
        --
        -- mask shorts
        mmA = mmAlt abKM
        mmH = mmHigh abKM
        mmL = mmLow abKM
        --
        xmkM = xmkMask abXMK
        --
        wsAllKeys = (wsShiftableKeys abWS) ++ (wsKeys abWS)
        --
        mkCCK  = closeHColapseLKill abMK
        --
        multiKeys =
          [ (0,
            -- close window
            [ (mkCCK  , kill1)
            -- go to next, previous non-empty workspace
            , (mkNESN , moveTo Next $ WSIs nonEmptyShitable)
            , (mkNESP , moveTo Prev $ WSIs nonEmptyShitable)
            -- focus master window
            , (mkMSR  , windows W.focusMaster)
            -- focus next, previous window
            , (mkNSN  , windows W.focusDown)
            , (mkPSF  , windows W.focusUp)
            -- float, unfloat
            , (mkUES  , withFocused $ windows . W.sink)
            -- prompt: goto window
            , (mkRRJ  , windowPromptGoto ppP)
            ])
          , (mmA,
            -- next, previous empty workspace
            [ (mkNESN , moveTo Next $ WSIs emptyShitable)
            , (mkNESP , moveTo Prev $ WSIs emptyShitable)
            ])
          , (mmA .|. mmL,
            -- next, previous empty workspace
            [ (mkNESN , moveTo Next $ WSIs emptyNonShitable)
            , (mkNESP , moveTo Prev $ WSIs emptyNonShitable)
            ])
          , (mmH,
            -- close other window copies
            [ (mkCCK  , killAllOtherCopies)
            -- send window to next, previous workspace
            , (mkNESN , shiftToNext)
            , (mkNESP , shiftToPrev)
            -- focus master window
            , (mkMSR  , windows W.swapMaster)
            -- swap with next, previous window
            , (mkNSN  , windows W.swapDown)
            , (mkPSF  , windows W.swapUp)
            -- expand master window
            , (mkUES  , sendMessage Expand)
            -- toggle structs
            , (mkBS   , sendMessage $ ToggleStruts)
            -- prompt: run or raise
            , (mkRRJ  , runOrRaisePrompt ppP)
            ])
          , (mmL,
            -- go to next, previous workspace
            [ (mkNESN , moveTo Next $ WSIs nonEmptyNonShitable)
            , (mkNESP , moveTo Prev $ WSIs nonEmptyNonShitable)
            -- shrink master window
            , (mkUES  , sendMessage Shrink)
            -- switch to next, first layout
            , (mkNSN  , sendMessage NextLayout)
            , (mkPSF  , sendMessage FirstLayout)
            -- reset layout
            , (mkMSR  , setLayout $ layoutHook conf)
            -- toggle borders
            , (mkBS   , withFocused toggleBorder >> refresh)
            -- prompt: run or raise
            , (mkRRJ  , shellPrompt ppP)
            ])
          -- xmonad
          , (xmkM,
            -- go to next, previous workspace
            [ (xmkQuit abXMK, io $ exitWith ExitSuccess)
            , (xmkRestart abXMK, restart "xmonad" True)
            ])
          ] where
            mkBS = borderHStruts abMK
            mkRRJ  = raiseHRunLJust abMK
            --
            mkNESN = nonAEmptyHShiftLNextAl abMK
            mkNESP = nonAEmptyHShiftLPreviousAl abMK
            --
            mkNSN = nextHShiftLNext abMK
            mkPSF = previousHShiftLFirst abMK
            --
            mkMSR = masterHShiftLReset abMK
            mkUES = unfloatHExpandLShrink abMK
            --
            ppP = ppPrompt abPP
        --
        isShiftable ws = wsIsTagShiftable abWS $ W.tag ws
        -- justOrNothing isShiftableFunction
        isEmptyShitable jon isf = return (\ws -> (jon $ W.stack ws ) && isf ws)
        emptyShitable = isEmptyShitable isNothing isShiftable
        emptyNonShitable = isEmptyShitable isNothing $ not.isShiftable
        nonEmptyShitable = isEmptyShitable isJust isShiftable
        nonEmptyNonShitable = isEmptyShitable isJust $ not.isShiftable
        --
        --
        mpdSpawnKeys =
          [ (mpkM,
            -- play / pause
            [ (mpkToglePlay abMPK , sMPTogglePlay)
            -- next, previous
            , (mpmkNFF , sMPNext)
            , (mpmkPFB , sMPPrevious)
            -- volume down, up
            , (mpkVolumeDown abMPK , sMPVolumeDown)
            , (mpkVolumeUp abMPK   , sMPVolumeUp)
            -- trash currently playing
            , (mpkMoveToTrash abMPK , sMPTrashCurrent)
            ])
          , ((mpkM .|. mpkF_wMask abMPK),
            -- fast backward, forward
            [ (mpmkPFB , sMPFastBackward)
            , (mpmkNFF , sMPFastForward)
            ])
          ] where
            mpkM  = mpkMask abMPK
            --
            mpmkNFF  = mpmkNextFFw abMPK
            mpmkPFB  = mpmkPreviousFBw abMPK
        --
        spawnMultiKeys =
          [ (0, -- mkTDS : leave to be used by guake or yakuake.
            -- blank the screen
            [ (mkBML , sXBlank)
            -- shot of focused window
            , (mkWSW , sWindowShot)
            -- toggle window transparency
            , (mkTID , sToggleTransparency)
            ])
          --, (Just mmA, [])
          , (mmH,
            -- system monitor
            [ (mkBML , sSystemMonitor)
            -- increase window transparency
            , (mkTID , sIncreaseTransparency)
            -- screen-shot of entire screen
            , (mkWSW , sScreenShot)
            -- open top drawer terminal
            , (mkTDS , sTopDrawerTerminal)
            ])
            -- run dmenu: CHECK THIS
            --    [ (xK_space, ("dmenu_path | dmenu " ++ abcDmenuOptions)) ])
          , (mmL,
            -- lock the screen
            [ (mkBML , sXLock)
            -- kill a window with the mouse
            , (mkCCK , sXKill)
            -- decrease window transparency
            , (mkTID , sDecreaseTransparency)
            -- reload wallpaper
            , (mkWSW, sReloadWallpaper)
            -- open configured terminal
            , (mkTDS , sTerminal)
            ])
          , (xmkM,
            -- configure xmonad
            [ (xK_e , sConfigureXmonad)
            -- compile xmonad.hs
            , (xmkCompile abXMK, sCompileXmonad)
            ])
          ] where
            mkBML  = blankHMonitorLLock abMK
            mkRTRT = runATerminalHRootAhTerminal abMK
            mkTDS  = terminalHDrawerLSinked abMK
            mkTID  = transparencyHIncreaseLDecrease abMK
            mkWSW  = windowHScreenshotLWallpapper abMK

