--------------------------------------------------------------------------------
-- | Module    :  XMonad.Actions.FloatMatrix
--
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--------------------------------------------------------------------------------
-- Float matrix... where windows dodge bullets.
--
-- Provides functions to manage floating windows using virtual matrixes of
-- 3x3 and 2x2, along with key binders (by default numpad based) that
-- provide full control over floating windows.
--
module XMonad.Actions.FloatMatrix where
--------------------------------------------------------------------------------
--
import XMonad.WheezyBugFixes (float)
--
--
import Data.Maybe (fromJust, isNothing)
import Data.Set (fromList)
--
--
import Graphics.X11.Xlib
  ( moveResizeWindow, moveWindow
  , Dimension, Position, Rectangle (..), Window
  )
--
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes (..))
--
--
import XMonad
  ( (.|.)
  , gets, io, windowset, withDisplay, withFocused, shiftMask
  , xK_KP_Home  , xK_KP_Up   , xK_KP_Page_Up
  , xK_KP_Left  , xK_KP_Begin, xK_KP_Right
  , xK_KP_End   , xK_KP_Down , xK_KP_Page_Down
  , xK_KP_Divide, xK_KP_Insert, xK_KP_Enter
  , X, ScreenDetail(SD), KeyMask, KeySym,
  )
--
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
--
import XMonad.Hooks.ManageDocks (calcGap)
import XMonad.Hooks.ManageHelpers (Side (NW, NC, NE, CW, C, CE, SW, SC, SE))
--
import XMonad.Util.Types (Direction2D (D, L, R, U))
--
import XMonad.StackSet (current, screenDetail, Screen(Screen))
--
--------------------------------------------------------------------------------
-- * Data and default instance
--
-- | The configuration data of a float matrix.
--
data FloatMatrix = FloatMatrix
  { ignoreStruts :: Bool -- ^ If true the float matrix will not avoid struts.
  --
  , gapWidth :: Dimension   -- ^ The gap arround each window.
  , margins  :: [Dimension] -- ^ The margins of the float area.
  , padding  :: Dimension   -- ^ The padding.
  --
  , keys  :: [KeySym]  -- ^ The keys to control the matrix.
  -- The list should contain 12 keys, the first 9 corresponding each to a
  -- side of the matrix ordered as 'matrixSides' is, followed by 3 more keys,
  -- 2 for horizontal and vertical actions, and 1 for 1/2 ratio actions.
  --
  , masks :: [KeyMask] -- ^ The key masks to control the matrix.
  -- The list should contain 7 diferent key masks that will associate with:
  -- side placing, spring resize, big move, litle move, increse size, decrease size
  -- and flod resize.
  -- Keep in mind that in a tipical configuration each mask given
  -- may be merged '(.|.)' with the global mask.
  --
  , onSpringPlace :: Maybe Side -- ^ Where to place the window when doing
  --   a spring resize, if 'Nothing' resizing will be centered on the window.
  , springCorner  :: Side -- ^ The corner for spring resizes, if not a corner,
    -- spring corner used will be: NW
  --
  , onFlodPlace :: Maybe Side -- ^ Same as 'onSpringPlace' but for flods.
  , flodCorner  :: Side       -- ^ Same as 'springCorner' but for flods.
  --
  , highMoveStep :: Dimension
  -- ^ The step size when moving windows with the high mask.
  , lowMoveStep  :: Dimension
  -- ^ The step size when moving windows with the low mask.
  , resizeStep   :: Dimension
  -- ^ The step size when resizing windows.
  }
--
-- | Take a look at the source for an example on how to configure
-- a 'FloatMatrix'.
--
defaultFloatMatrix
  :: [KeyMask] -- ^ A list containing 4 pure key masks.
  -> FloatMatrix -- ^ A default instance of 'FloatMatrix' based on the given mask list.
--
defaultFloatMatrix [mm0, mmA, mmH, mmL] = FloatMatrix
  --
  { ignoreStruts = False
  --
  , gapWidth = 0
  , margins  = [0, 0, 0, 0]
  , padding  = 0
  --
  , keys =
    -- NW, NC, NE
    [ xK_KP_Home, xK_KP_Up   , xK_KP_Page_Up
    -- CW, C, CE
    , xK_KP_Left, xK_KP_Begin, xK_KP_Right
    -- SW, SC, SE
    , xK_KP_End , xK_KP_Down , xK_KP_Page_Down
    -- horizontal half, vertical half, half
    , xK_KP_Insert, xK_KP_Enter, xK_KP_Divide
    ]
  --
  , masks =
    [ mm0
    , mmA, mmA .|. mmH, mmA .|.  mmL
    , mmH, mmL, mmH .|. mmL
    ]
  --
  , flodCorner   = NW
  , springCorner = SW
  --
  , onFlodPlace = Just NW
  , onSpringPlace   = Just C
  --
  , highMoveStep = 100
  , lowMoveStep  = 10
  , resizeStep   = 35
  }
--
--------------------------------------------------------------------------------
-- * End user functions
--
flatBindingsTree
  :: KeyMask -- ^ The key mask common to all bindings.
  -> [(KeyMask, [(bOKs, a)])] -- ^ The input bindings tree,
    -- bOKs: something like 'Button' or 'KeySym',
    -- a: something like (Window -> X ()) or X()
  -> [((KeyMask, bOKs), a)] -- ^ The flatten bindings list.
--
flatBindingsTree km msall =
  concatMap (\(m, sal) -> map (\(s, a) -> ((km .|. m, s), a)) sal) msall
--
--
flatBindingsTreeToFocused
  :: KeyMask -- ^ The common key mask.
  -> [(KeyMask, [(bOKs, (Window -> X ()))])] -- ^ The input bindings tree,
    -- bOKs: something like 'Button' or 'KeySym'.
  -> [((KeyMask, bOKs), X ())] -- ^ The flatten bindings list.
--
flatBindingsTreeToFocused km msxll =
  map (\(kt, wx) -> (kt, withFocused wx)) $ flatBindingsTree km msxll
--
--
matrixBindings
  :: FloatMatrix -- ^ The float matrix
  -> (Window -> X()) -- ^ REMOVE: bigStepFunction -> temporary maximize
  -> (Window -> X()) -- ^ REMOVE: smallStepFunction ->
  -> [(KeyMask, [(KeySym, (Window -> X ()))])] -- ^ The key bindings
    -- of actions over a window for the given float matrix.
--
matrixBindings fm bsf00 ssf00 = zip (masks fm)
  [ zK $ map (floatTo fm) matrixSides
  --
  , zKFS onSpringPlace springCorner springSizeRatios
  --
  , zKM highMoveStep bsf00
  , zKM lowMoveStep ssf00
  --
  , zKR fmRS
  , zKR $ -fmRS
  --
  , zKFS onFlodPlace flodCorner flodSizeRatios
  ]
  where
    fmRS = resizeStep fm
    -- zip Keys: fmFloatList
    zK fmfl = zip (keys fm) $ fmfl
    -- zip Keys Float/Spring: fmOnFun, fmCornerFun, ratiosFun
    zKFS fmof fmcf rf = zK $ map (
      if isNothing $ fmof fm
        then floatMatrixRatioResize fm
        else floatMatrixPlace fm $ fromJust $ fmof fm
      ) $ rf $ fmcf fm
    --
    -- zip Key Moves
    zKM fmmsf = zK.moveActions (fmmsf fm)
    -- zip Key Resizes
    zKR rs = zK $ allResizeActions rs
--
--------------------------------------------------------------------------------
-- * Base constructructs
--
matrixSides
  :: [Side] -- ^ A list with the 9 ordered sides of the matrix.
--
matrixSides = [NW, NC, NE, CW, C , CE, SW, SC, SE]
--
--
sizeRatioMatrix
  :: Side       -- ^ Origin corner side, if not a corner will default to NW
  -> [Rational] -- ^ Size ratio list
  -> [(Rational, Rational)] -- ^ A size ratio matrix of 3x3 with origin at
    -- the given corner in a list suitable to zip with 'matrixSides'.
--
sizeRatioMatrix ocs srl = case ocs of
  NW -> bm srl srl
  NE -> bm rsrl srl
  SW -> bm srl rsrl
  SE -> bm rsrl rsrl
  _  -> sizeRatioMatrix NW srl
  where
    -- reversed size ratios list
    rsrl = reverse srl
    -- build matrix: width ratio list, height ratio list
    bm wrl hrl = [(wr, hr) | hr <- hrl, wr <- wrl]
--
--
springSizeRatios
  :: Side -- ^ Origin corner side
  -> [(Rational, Rational)] -- ^ The spring ratios for the given origin,
    -- a list with the 'sizeRatioMatrix'.
--
springSizeRatios ocs = sizeRatioMatrix ocs (map (\r -> r/3) [1..3]) ++
  [ (1, 1/2), (1/2, 1), (1/2, 1/2) ]
--
--
flodSizeRatios
  :: Side -- ^ Origin corner side
  -> [(Rational, Rational)] -- ^ The flod ratios for the given origin.
--
flodSizeRatios ocs = sizeRatioMatrix ocs [1..3] ++
  [ (2, 1), (1, 2), (2, 2) ]
--
--------------------------------------------------------------------------------
-- * Actions based on 'FloatKeys'
--
moveActions
  :: Dimension -- ^ The move step size.
  -> (Window -> X ()) -- ^ A custom middle function (would move to nowhere).
  -> [Window -> X ()] -- ^ A list of 9 move functions over a given window,
    -- suitable to zip with 'keys'.
--
moveActions s f00 = map mf [(h, v) | v <- sl, h <- sl] where
  -- stepList
  sl = [-s, 0, s]
  -- moveFunction stepTouple
  mf st = case st of
    (0, 0) -> f00
    (_, _) -> keysMoveWindow st
--
--
allResizeActions
  :: Dimension -- ^ The resize step size
  -> [Window -> X ()] -- ^ A list of 11 resize functions over a given window,
    -- suitable to zip with 'keys'.
--
allResizeActions rs =
  ms resizeActions rs ++ (ms hvResizeActions $ rs * 2)
  where ms ral s = map (\f -> f s) ral -- map steps
--
--
resizeActions
  :: [Dimension -> Window -> X ()] -- ^ A list of 9 resize functions
    -- over a given window, suitable to prepend before
    -- 'hvResizeActionBindings' for zipping with 'keys'.
--
resizeActions =
  [ r 1 1, v    1   , r 0 1
  , h  1 , r 0.5 0.5, h  0
  , r 1 0, v    0   , r 0 0
  ] where
  -- resize
  r h@0.5 v@0.5 s = kRW ds ds h v -- double resize for middle.
    where ds = s * 2
  r h v s = kRW s s h v
  -- ... horizontally
  h = kRWHorizontal
  -- ... vertically
  v = kRWVertical
--
--
hvResizeActions
  :: [Dimension -> Window -> X ()] -- ^ A list of 2 resize functions over
    -- a given window, suitable to append to 'resizeActionBindings' before
    -- zipping with 'keys'.
--
hvResizeActions = [kRWHorizontal 0.5, kRWVertical 0.5]
--
--
-- | Horizontal step resize stub to 'keysResizeWindow'
kRWHorizontal :: Rational -> Dimension -> Window -> X ()
--
kRWHorizontal h s = kRW s 0 h 0
--
--
-- | Vertical step resize stub to 'keysResizeWindow'
kRWVertical :: Rational -> Dimension -> Window -> X ()
--
kRWVertical v s = kRW 0 s 0 v
--
--
-- | Untoupled stub to 'keysResizeWindow'
kRW dx dy h v = keysResizeWindow (dx, dy) (h, v)
--
--------------------------------------------------------------------------------
--
resize
  :: Dimension -- ^ Step size.
  -> Window -- ^ The target window.
  -> X() -- ^ The step resize action.
--
resize sw w = withDisplay $ \d -> do
  wa <- io $ getWindowAttributes d w
  mrw d $ calcCenteredResize wa (r wa_width wa, r wa_height wa)
  float w where
    fi f ni = fromIntegral $ f ni
    r df wa = (fi df wa) + ((fi wa_border_width wa) * 2) + sw
    mrw d (wx, wy, ww, wh) = io $ moveResizeWindow d w wx wy ww wh
--
--------------------------------------------------------------------------------
-- * Matrix resize
--
matrixResize
  :: ((Rectangle -> X()) -> X()) -- ^ with.. function,
    -- like: 'withFloatMatrix', 'withScreenArea' or 'withStrutsFreeArea'.
  -> FloatMatrix -- ^ A float matrix.
  -> (Rational, Rational) -- ^ The new size, relative to the float area.
  -> Window -- ^ The target window.
  -> X() -- ^ The ratio resize action.
--
matrixResize wf fm r w = withDisplay $ \d -> do
  wa <- io $ getWindowAttributes d w
  wf $ twF d wa where
    fi ni = fromIntegral ni
    twF d wa far = do
      mrw $ calcCenteredResize wa
        $ calcRatioResize far (gapWidth fm) (fi $ wa_border_width wa) r
      float w where
        mrw (wx, wy, ww, wh) = io $ moveResizeWindow d w wx wy ww wh
--
--
-- | Short to: 'matrixResize' ('withFloatMatrix' fm) fm
floatMatrixRatioResize
  :: FloatMatrix -- ^ fm
  -> (Rational, Rational)
  -> Window
  -> X()
--
floatMatrixRatioResize fm = matrixResize (withFloatMatrix fm) fm
--
--
-- | Short to: 'matrixResize' 'withScreenArea'
screenRatioResize
  :: FloatMatrix
  -> (Rational, Rational)
  -> Window
  -> X()
--
screenRatioResize = matrixResize withScreenArea
--
--------------------------------------------------------------------------------
-- * Matrix side moves
--
floatTo
  :: FloatMatrix -- ^ A float matrix.
  -> Side -- ^ The side where to float the window.
  -> Window -- ^ The target window.
  -> X() -- ^ An action that moves the window to the given side.
--
floatTo fm s w = withDisplay $ \d -> do
  wa <- io $ getWindowAttributes d w
  withFloatMatrix fm $ twF d wa where
    twF d wa far = do
      mrw $ calcSideXY far (gapWidth fm)
        (fwa wa_border_width) (fwa wa_width, fwa wa_height) s
      float w
      where
        fwa f = fromIntegral $ f wa
        mrw (x, y) = io $ moveWindow d w x y
--
--------------------------------------------------------------------------------
-- * Place in matrix
--
matrixPlace
  :: ((Rectangle -> X()) -> X()) -- ^ with.. function,
    -- like: 'withFloatMatrix', 'withScreenArea' or 'withStrutsFreeArea'.
  -> FloatMatrix -- ^ A float matrix.
  -> Side -- ^ The side where to place the window.
  -> (Rational, Rational) -- ^ The new size of the window.
  -> Window -- ^ The target window.
  -> X() -- ^ An action that floats and resizes the given window
    -- to the given side and resized to the given ratio.
--
matrixPlace wf fm s sr w = withDisplay $ \d -> do
  wa <- io $ getWindowAttributes d w
  wf $ twF d wa where
  twF d wa far = do
    mrw $ calcSideXY far gw bw wh s
    float w where
    -- fwa f = fromIntegral $ f wa
    wh = calcRatioResize far gw bw sr
    bw = fromIntegral $ wa_border_width wa
    gw = gapWidth fm
    mrw (x, y) = io $ moveResizeWindow d w x y (fst wh) (snd wh)
--
--
-- | Short to: 'matrixPlace' (withFloatMatrix fm) fm
floatMatrixPlace
  :: FloatMatrix -- ^ fm
  -> Side -> (Rational, Rational) -> Window -> X()
--
floatMatrixPlace fm = matrixPlace (withFloatMatrix fm) fm
--
--
-- | Short to:
-- >   'matrixPlace' 'withScreenArea'
screenMatrixPlace
  :: FloatMatrix -> Side -> (Rational, Rational) -> Window -> X()
--
screenMatrixPlace = matrixPlace withScreenArea
--
--------------------------------------------------------------------------------
-- * Action area selectors
--
withFloatMatrix
  :: FloatMatrix -- ^ The float matrix.
  -> (Rectangle -> X()) -- ^ A function that will accept a rectangle
    -- representing the float area and does something with it.
  -> X() -- ^ An action based on the given function.
--
withFloatMatrix fm f = do
  (if ignoreStruts fm then withScreenArea else withStrutsFreeArea)
    $ stubToFM (margins fm) (padding fm)
  where
    fi ni = fromIntegral ni
    wh a b = a - b
    stubToFM [mt, mr, mb, ml] p far = f $ Rectangle
      (rect_x far + (fi $ ml + p)) (rect_y far + (fi $ mt + p))
      (wh (rect_width far) $ ml + mr + p2)
      (wh (rect_height far) $ mt + mb + p2)
      where p2 =  (p * 2)
--
--
-- | Similar to 'withFloatMatrix' but for the struts free area.
withStrutsFreeArea :: (Rectangle -> X()) -> X()
--
withStrutsFreeArea f = do
  -- screen area rectangle -> struts free area rectangle
  sar2sfar <- calcGap $ fromList [D, L, R, U]
  withScreenArea (\sar -> f $ sar2sfar sar)
--
--
-- | Similar to 'withFloatMatrix' but for the entire screen area.
withScreenArea :: (Rectangle -> X()) -> X()
--
withScreenArea rxf = do
  sar <- gets $ sdr . current . windowset -- screen area rectangle
  rxf sar
  where -- screen detail rectangle
    sdr (Screen {screenDetail = (SD r)}) = r --  W.screen = id,
--
--------------------------------------------------------------------------------
-- * Position and size calculation helpers
--
calcCenteredResize
  :: WindowAttributes -- ^ The target window attributes.
  -> (Dimension, Dimension) -- ^ The new desire size.
  -> (Position, Position, Dimension, Dimension) -- ^ The new position and
    -- size for the window.
calcCenteredResize wa (nw, nh) = (x, y, nw, nh) where
  x = xy rect_x wa_x nw wa_width
  y = xy rect_y wa_y nh wa_height
  --
  xy fxf wxf nd wdf =
    (fi (wxf wa) - ((fi nd - (fi $ wdf wa)) `div` 2)) where
      fi ni = fromIntegral ni
--
--
calcRatioResize
  :: Rectangle -- ^ The rectangle of the float area.
  -> Dimension -- ^ The gap width arround the window.
  -> Dimension -- ^ The window border width.
  -> (Rational, Rational) -- ^ The wanted size for the window,
    -- relative to the given area, including border and gap.
  -> (Dimension, Dimension) -- ^ The new size for the window
    -- without border or gap.
--
calcRatioResize far gw bw (wr, hr) = (c wr rect_width, c hr rect_height) where
  c r df = (round $ r * (toRational $ df far)) - (gw * 2) - (bw * 2)
--
--
calcSideXY
  :: Rectangle -- ^ The rectangle of the float area.
  -> Dimension -- ^ The gap width arround the window.
  -> Dimension -- ^ The window border width.
  -> (Dimension, Dimension) -- ^ the size of the window,
    -- without border or gap.
  -> Side -- ^ The side where to place the window.
  -> (Position, Position) -- ^ The new coordinates (x, y) for the window.
--
calcSideXY far gw bw (w, h) side = (x, y) where
  fi x = fromIntegral x
  cd df d = fi $ df far - d - (gw * 2) - (bw * 2) -- calculateDimension
  wd = cd rect_width w -- widthDimension
  hd = cd rect_height h -- heightDimension
  x = xy rect_x [SC,C ,NC] [SW,CW,NW] wd -- [SE,CE,NE]
  y = xy rect_y [CE,C ,CW] [NE,NC,NW] hd -- [SE,SC,SW]
  --
  xy rf fsl ssl d = rf far + (fi gw) +
    if side `elem` fsl
        then d `div` 2
        else if side `elem` ssl
        then 0
        else d -- side `elem` [SE,CE,NE] / [SE,SC,SW]
--
--------------------------------------------------------------------------------
--
