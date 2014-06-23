--------------------------------------------------------------------------------
-- | Module    :  XMonad.Config.AlphaMinD
--
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--------------------------------------------------------------------------------
-- Provides a more advanced 'ABC' configuration.
--
module XMonad.Config.AlphaMinD where
--------------------------------------------------------------------------------
--
import XMonad.Config.ABC
import XMonad.Config.ABCDefaults
--
-- change keyboard, add programming keys
-- http://askubuntu.com/questions/37491/how-do-i-change-my-keyboard-layout-while-using-xmonad
-- http://blog.alphabit.org/2013/01/there-you-go.html
--  , sConfigureXmonad = sTDT ++ " -x mc -e ~/.xmonad/ABXM/AlphaMinD.hs"
-- help in autostart and key to reinvoke it: mmAL
-- open nload, alsamixer,
  --, sCheckRequired  ::
  --  ["feh", "ghc", "mc", "mpc", "scrot", "top", "transset", "xtrlock" "xmessage"]

--
{-- main
main = do
  --
  -- log spawn pipe
  lsp <- spawnPipe $ spawnLogPipe abxmDefault
  --
  -- start xmonad
  xmonad $ abXMConfig gnomeConfig abxmDefault lsp
  -}
--
abGnomeSpawns = abSpawns {
    sStartup = abBuildAutostartSpawn
      "zenity --title $xa --info --text=\"$text\";" -- show it...
  --
  , sConfigureXmonad = "gedit ~/.xmonad/xmonad.hs"
  --
  , sSystemMonitor   = "gnome-system-monitor"
  }

abGnomeMediaPlayerSpawns = abMediaPlayerSpawns
  {
   sMPTrashCurrent = "zenity --warning --title 'Moved to mpd-trash' --text=`"
    ++ sMPTrashCurrent abMediaPlayerSpawns ++ "`"
  }
--
--------------------------------------------------------------------------------
--
abKDESpawns = abSpawns {
    sStartup = abBuildAutostartSpawn
      "NOTzenity --title $xa --info --text=\"$text\";" -- show it...
  --
  , sConfigureXmonad = "kate ~/.xmonad/xmonad.hs"
  --
  , sSystemMonitor   = "ksysguard"

  -- screenshot = kscreensh
  }
