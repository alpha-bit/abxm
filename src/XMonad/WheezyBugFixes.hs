--------------------------------------------------------------------------------
-- Module      :  ABXM.XMonadWheezyBugFixes
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- | This module contains bug fixes to XMmonad in wheezy.
-- use with import 'XMonad' hiding ('floatLocation', 'float')
--------------------------------------------------------------------------------
--
module XMonad.WheezyBugFixes where
--
--
import Control.Applicative ((<$>))
import Control.Monad (guard)
--
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
--
import XMonad hiding (floatLocation, float)
--
import qualified XMonad.StackSet as SS
--
-- | Given a window, find the screen it is located on, and compute
-- the geometry of that window wrt. that screen.
floatLocation :: Window -> X (ScreenId, SS.RationalRect)
floatLocation w = withDisplay $ \d -> do
    ws <- gets windowset
    wa <- io $ getWindowAttributes d w
    -- bw <- fi <$> asks (borderWidth . config) -- abc
    sc <- fromMaybe (SS.current ws) <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)

    let sr = screenRect . SS.screenDetail $ sc
        rr = SS.RationalRect
          ((fi (wa_x wa) - fi (rect_x sr)) % fi (rect_width sr))
          ((fi (wa_y wa) - fi (rect_y sr)) % fi (rect_height sr))
          (fi (wa_width  wa + bw*2) % fi (rect_width sr))
          (fi (wa_height wa + bw*2) % fi (rect_height sr))
        bw = wa_border_width wa -- abc

    return (SS.screen sc, rr)
  where fi x = fromIntegral x
--
--
-- | Make a tiled window floating, using its suggested rectangle
float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws -> SS.float w rr . fromMaybe ws $ do
        i  <- SS.findTag w ws
        guard $ i `elem` map (SS.tag . SS.workspace) (SS.screens ws)
        f  <- SS.peek ws
        sw <- SS.lookupWorkspace sc ws
        return (SS.focusWindow f . SS.shiftWin sw w $ ws)
--
--

