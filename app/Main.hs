{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.State (State, execState, get)
import Control.Monad.Reader (MonadReader, Reader, runReader, ask)
import Control.Monad (when)
import System.Exit (die)
import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (delete)
import Data.Maybe (maybeToList)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, encodeFile, decodeFileStrict)
import qualified Data.ByteString.Lazy as B
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Image hiding (normalize, translate, scale)
import Linear
import GHC.Float.RealFracMethods (float2Int)
import Text.Printf (printf)
import Debug.Trace


---------------------
-- Const
---------------------

minGridSizePxl = 6 -- [pxl]
gridSizeList = [100, 1000,10000,10000] -- [mm]
defaultSSize = (1300, 700) -- [pxl]
wallThickness = 150  -- [mm]
wallHeight    = 2500 -- [mm]
maxmmpxl = 1024
minmmpxl = 8
marginPxl = 3



---------------------
-- Utility
---------------------

wpt = V2

wpt2tup (V2 x y) = (x, y)

tup2wpt (x, y) = V2 x y

f2i = float2Int

i2f = fromIntegral

clamp l u x = min u (max l x)

scaleUp :: Float -> Float
scaleUp x = clamp minmmpxl maxmmpxl $ x * 2

scaleDown :: Float -> Float
scaleDown x = clamp minmmpxl maxmmpxl $ x / 2



---------------------
-- State
---------------------

type WPoint = V2 Float -- [mm] NOTE: WPoint represents points in world coordinates

instance FromJSON a => FromJSON (V2 a)
instance ToJSON a => ToJSON (V2 a)


data Wall = Wall WPoint WPoint Float deriving (Show, Eq, Generic) -- start end thickness

instance FromJSON Wall
instance ToJSON Wall


data Mode
    = ViewMode [Wall]  -- selected wall
    | SlideMode WPoint -- cached translation origin
    | BuildMode
    | WallMode WPoint  -- start point of wall
    | EraseMode deriving Show

makePrisms '' Mode


data Screen = Screen
    { _center :: WPoint   -- center point of screen [mm]
    , _mmpxl :: Float     -- scale [mm/pxl] <= 1000
    , _ssize :: (Float, Float) -- screen size
    , _focus :: WPoint   -- mmouse pointer on screen
    } deriving Show

makeLenses ''Screen

defaultScreen :: Screen
defaultScreen = Screen (wpt 0 0) 128 defaultSSize (wpt 0 0)


data Model = Model
    { _walls :: [Wall]
    } deriving (Show, Eq, Generic)

makeLenses ''Model

instance FromJSON Model
instance ToJSON Model

defaultModel :: Model
defaultModel = Model []


data App = App
    { _model :: Model -- domain model
    , _screen  :: Screen
    , _mode :: Mode
    , _prevModels :: [Model]
    , _nextModels :: [Model]
    } deriving Show

makeLenses ''App

defaultS :: Model -> App
defaultS model = App model defaultScreen (ViewMode []) [] []

---------------------
-- Draw Utility
---------------------


gridSize :: (MonadReader Screen m) =>  m Float
gridSize = do
    v <- ask
    return $ head [x | x <- gridSizeList, x >= minGridSizePxl * (v ^. mmpxl)]

align :: Screen -> WPoint -> WPoint
align s pt = run s $ do
    gs <- gridSize
    let (x, y) = wpt2tup pt
    return $ wpt (gs * (i2f . round) (x / gs)) (gs * (i2f . round) (y / gs))
    where run = flip runReader

align' :: Screen -> Point -> WPoint
align' s pt = align s $ s2w' s pt

w2s :: (MonadReader Screen m) =>  WPoint -> m Point
w2s p = do
    v <- ask
    let (w, h) = v ^. ssize
    return $ wpt2tup $ (p - (v ^. center)) ^/ (v ^. mmpxl)


s2w' :: Screen -> Point -> WPoint
s2w' s p = runReader (s2w p) s

s2w :: (MonadReader Screen m) => Point -> m WPoint
s2w p = do
    v <- ask
    let (w, h) = v ^. ssize
    return $ tup2wpt p ^* (v ^. mmpxl) + (v ^. center)


leftTop :: (MonadReader Screen m) => m WPoint
leftTop = do
    (w, h) <- view ssize
    s2w (-w/2, -h/2)


rightBottom :: (MonadReader Screen m) => m WPoint
rightBottom = do
    (w, h) <- view ssize
    s2w (w/2, h/2)


drawThickLine :: Point -> Point -> Float -> Picture
drawThickLine (x0, y0) (x1, y1) thickness
    | thickness <= 0 = line [(x0, y0), (x1, y1)]
    | otherwise = polygon [p00, p01, p11, p10]
        where
        c0 = wpt x0 y0
        c1 = wpt x1 y1
        v = normalize $ wpt (y1 - y0) (x0 - x1)
        p00 = wpt2tup $ c0 + v ^* (thickness / 2)
        p01 = wpt2tup $ c0 - v ^* (thickness / 2)
        p10 = wpt2tup $ c1 + v ^* (thickness / 2)
        p11 = wpt2tup $ c1 - v ^* (thickness / 2)


drawThickLineW :: (MonadReader Screen m) => WPoint -> WPoint -> Float -> m Picture
drawThickLineW p0 p1 thickness = drawThickLine <$> w2s p0 <*> w2s p1 <*> ((thickness /) <$> view mmpxl)


drawText :: (MonadReader Screen m) => (Float, Float) -> String -> m Picture
drawText (rx, ry) s = do
    (w, h) <- view ssize
    return $ translate (-w/2 + w * rx) (h/2  - h * ry) $ scale 0.1 0.1 $ text s
    --return $ translate (-w/2 + w * 0.1) (h/2  - h * 0.1) $ scale 0.1 0.1 $ text s



---------------------
-- Draw
---------------------

drawMode :: (MonadReader Screen m) => Mode -> m Picture
drawMode m = do
    (w, h) <- view ssize
    return $ translate (-w/2+10) (h/2 - 20) $ scale 0.1 0.1 $ text $ show m


drawBackground :: (MonadReader Screen m) => m Picture
drawBackground = do
    (w, h) <- view ssize
    return $ color white $ rectangleSolid w h


drawGrid :: (MonadReader Screen m) => m Picture
drawGrid = do
    (x0, y0) <- wpt2tup <$> leftTop
    (x1, y1) <- wpt2tup <$> rightBottom
    gs <- gridSize
    mp <- view mmpxl
    let [xi0, yi0]  = [f2i gs * ceiling (x / gs) | x <- [x0, y0]]
    let [xi1, yi1]  = [f2i gs * floor (x / gs) | x <- [x1, y1]]
    let xs = enumFromThenTo xi0 (xi0 + f2i gs) xi1
    let ys = enumFromThenTo yi0 (yi0 + f2i gs) yi1
    xlines <- sequence [drawThickLineW (wpt x' y0) (wpt x' y1) t | x <- xs, let x' = i2f x, let t = if f2i (x' / gs) `mod` 10 == 0 then 2 * mp else 0]
    ylines <- sequence [drawThickLineW (wpt x0 y') (wpt x1 y') t | y <- ys, let y' = i2f y, let t = if f2i (y' / gs) `mod` 10 == 0 then 2 * mp else 0]
    return $ color (greyN 0.8) $ pictures $ xlines ++ ylines


drawWall :: (MonadReader Screen m) => Color -> Wall -> m Picture
drawWall c (Wall p0 p1 t) = color c <$> drawThickLineW p0 p1 t

drawOrigin :: (MonadReader Screen m) => m Picture
drawOrigin =  do
    (x, y) <- w2s $ wpt 0 0
    return $ translate x y $ color (greyN 0.8) $ thickCircle 2 4

draw :: App -> IO Picture
draw s = do
    let v = s ^. screen
    (w, h) <- getScreenSize
    let run = flip runReader $ v & ssize .~ (i2f w, i2f h)
    let gs = run gridSize
    let tmpWall = maybeToList $ drawWall red <$> (\e -> Wall (align (s ^. screen) (s ^. screen . focus)) e wallThickness) <$> s ^? mode . _WallMode
    let selectedWalls = drawWall red <$> concat (maybeToList $ s ^? mode . _ViewMode)
    let pics = run $ sequence $
            [ drawBackground
            , drawGrid
            , drawText (0.01, 0.03) $ show $ s ^. mode
            , drawText (0.01, 0.06) $ "grid : " ++ show (f2i gs) ++ " [mm]"
            , drawText (0.01, 0.09) $ "grid-thick : " ++ show (f2i $ 10 * gs / 1000) ++ " [m]"
            , drawText (0.01, 0.12) $ "mm/pxl : " ++ show (v ^. mmpxl)
            , drawText (0.01, 0.15) $ "x, y : " ++ show (v ^. focus / 1000) ++ " [m]"
            , drawOrigin
            ] ++ [drawWall black w | w <- s ^. model . walls] ++ tmpWall ++ selectedWalls
    return $ Pictures pics


---------------------
-- Event
---------------------

handleInput :: Event -> App -> IO App
handleInput ev s
    | is _ViewMode $ s ^. mode = handleInputViewMode ev s
    | is _SlideMode $ s ^. mode = handleInputSlideMode ev s
    | is _BuildMode $ s ^. mode = handleInputBuildMode ev s
    | is _WallMode $ s ^. mode = handleInputWallMode ev s
    -- | is _EraseMode $ s ^. mode = handleInputEraseMode ev s
    | otherwise = return s


handleInputCommon :: Event -> App -> IO App
handleInputCommon (EventKey (Char '\DC1') Down (Modifiers Up Down Up) _) s = die "exit" -- ctrl-q
handleInputCommon (EventKey (Char '\NAK') Down (Modifiers Up Down Up) _) s = return $ undo s
handleInputCommon (EventKey (Char '\DC2') Down (Modifiers Up Down Up) _) s = return $ redo s
handleInputCommon (EventKey (Char '\DC3') Down (Modifiers Up Down Up) _) s = do
    writeFile "model.sdf" $ toXML "room2" $ s ^. model
    encodeFile "model.json" $ s^. model
    dumpPng "obsmap.png" $ s ^. model
    return s
handleInputCommon (EventKey (MouseButton WheelDown) Up _ _) s = return $ s & screen . mmpxl %~ scaleUp
handleInputCommon (EventKey (MouseButton WheelUp) Up _ _) s = return $ s & screen . mmpxl %~ scaleDown
handleInputCommon (EventMotion pt) s = return $ s & screen . focus .~ align' (s ^. screen) pt
handleInputCommon ev s = return s


handleInputViewMode :: Event -> App -> IO App
handleInputViewMode (EventKey (Char 'w') Down _ pt) s = return $ s &  mode .~ BuildMode
handleInputViewMode (EventKey (SpecialKey KeyEsc) Down _ _) s = return $ s &  mode .~ ViewMode []
handleInputViewMode (EventKey (Char '\b') Down _ _) s = return $ s &~ do
    let w = s ^?! mode . _ViewMode . _head
    model . walls %= delete w
    mode .= ViewMode []
    prevModels %= (s ^. model:)
    nextModels .= []
handleInputViewMode (EventKey (MouseButton LeftButton) Down _ pt) s = do
    let selectedWall = findSelectedWall (s ^. model . walls) (s2w' (s ^. screen) pt) (marginPxl * s ^. screen . mmpxl)
    case selectedWall of
        Just w  -> return $ s &  mode .~ ViewMode [w]
        Nothing -> return $ s &  mode .~ (SlideMode $ align' (s ^. screen) pt)

handleInputViewMode ev s = handleInputCommon ev s


handleInputSlideMode :: Event -> App -> IO App
handleInputSlideMode (EventKey (MouseButton LeftButton) Up _ pt) s = return $ s &  mode .~ ViewMode []
handleInputSlideMode (EventMotion pt) s = return $ s &~ do 
    let SlideMode p0 = s ^.  mode
    let cur = s2w' (s ^. screen) pt
    screen . center += (p0 - cur)
    screen . focus .= cur
handleInputSlideMode ev s = handleInputCommon ev s


handleInputBuildMode :: Event -> App -> IO App
handleInputBuildMode (EventKey (SpecialKey KeyEsc) Down _ pt) s = return $ s &  mode .~ ViewMode []
handleInputBuildMode (EventKey (MouseButton LeftButton) Down _ pt) s = return $ s &  mode .~ (WallMode $ align' (s ^. screen) pt)
handleInputBuildMode ev s = handleInputCommon ev s


handleInputWallMode :: Event -> App -> IO App
handleInputWallMode (EventKey (MouseButton LeftButton) Up _ pt) s = return $ s &~ do
    let WallMode start = s ^.  mode
    let end = align' (s ^. screen) pt
    mode .= BuildMode
    when (distance start end > 0) $ do
            prevModels %= (s ^. model:)
            nextModels .= []
            model . walls %= (Wall start end wallThickness :)
handleInputWallMode (EventKey (SpecialKey KeyEsc) Down _ pt) s = return $ s &  mode .~ BuildMode
handleInputWallMode ev s = handleInputCommon ev s



---------------------
-- App Utility
---------------------

undo :: App -> App
undo s
    | length (s ^. prevModels) > 0 = s &~ do
                                            nextModels %= (s ^. model:)
                                            model .= head (s ^. prevModels)
                                            prevModels %= tail
    | otherwise = s

redo :: App -> App
redo s
    | length (s ^. nextModels) > 0 = s &~ do
                                            prevModels %= (s ^. model:)
                                            model .= head (s ^. nextModels)
                                            nextModels %= tail
    | otherwise = s


isWallSelected :: Wall -> WPoint -> Float -> Bool
isWallSelected (Wall p0 p1 t) p delta = proj_between_p0_and_p1 && l_p_proj <= t / 2 + delta
    where
    proj = p0 + project (p1 - p0) (p - p0)
    l_p0_p1 = distance p0 p1
    l_p0_proj = distance p0 proj
    l_p_proj = distance p proj
    d = dot (proj - p0) (p1 - p0)
    proj_between_p0_and_p1 = 0 <= d && d <= l_p0_p1 ** 2

findSelectedWall :: [Wall] -> WPoint -> Float -> Maybe Wall
findSelectedWall ws p delta = case [w | w <- ws, isWallSelected w p delta] of
    [] -> Nothing
    x:_ -> Just x


---------------------
-- File IO
---------------------


xml  = "<?xml version='1.0'?>                                                       \n\
       \<sdf version='1.6'>                                                         \n\
       \   <model name='%s'>                                                        \n\
       \     <pose frame=''>0 0 0 0 0 0</pose>                                      \n\
       \     %s                                                                     \n\
       \     <static>1</static>                                                     \n\
       \   </model>                                                                 \n\
       \</sdf>"


wallxml = "     <link name='Wall_%d'>                                                  \n\
          \       <collision name='Wall_%d_Collision'>                                 \n\
          \         <geometry>                                                         \n\
          \           <box>                                                            \n\
          \             <size>%f %f %f</size>                                          \n\
          \           </box>                                                           \n\
          \         </geometry>                                                        \n\
          \         <pose frame=''>0 0 0 0 0 0</pose>                                  \n\
          \       </collision>                                                         \n\
          \       <visual name='Wall_0_Visual'>                                        \n\
          \         <pose frame=''>0 0 0 0 0 0</pose>                                  \n\
          \         <geometry>                                                         \n\
          \           <box>                                                            \n\
          \             <size>%f %f %f</size>                                          \n\
          \           </box>                                                           \n\
          \         </geometry>                                                        \n\
          \         <material>                                                         \n\
          \           <script>                                                         \n\
          \             <uri>file://media/materials/scripts/gazebo.material</uri>      \n\
          \             <name>Gazebo/Grey</name>                                       \n\
          \           </script>                                                        \n\
          \           <ambient>1 1 1 1</ambient>                                       \n\
          \         </material>                                                        \n\
          \         <meta>                                                             \n\
          \           <layer>0</layer>                                                 \n\
          \         </meta>                                                            \n\
          \       </visual>                                                            \n\
          \       <pose frame=''>%f %f 0 0 0 %f</pose>                                 \n\
          \     </link>                                                                \n"




toXML :: String -> Model -> String
toXML name model = printf xml name $ concat [dumpWall idx w | (idx, w) <- zip [0..] (model ^. walls)]
    where
    dumpWall :: Int -> Wall -> String
    dumpWall idx (Wall start end thickness) =
            printf wallxml idx idx bx by bz bx by bz cx cy angle
            where
            (cx, cy) = wpt2tup $ (start + end) / 2 / 1000
            (dx, dy) = wpt2tup $ end - start
            angle = atan2 dy dx
            (bx, by, bz) = (distance start end / 1000, thickness/1000, wallHeight/1000 :: Float)


dumpPng :: String -> Model -> IO ()
dumpPng filepath model = B.writeFile filepath $ encode PNG [] obsMap
    where
    obsMap = makeImageR VS (w, h) f :: Image VS Y Word8
    idx2wpt r c = wpt ((fromIntegral c  - w / 2) * mmpxl') ((h / 2 - fromIntegral r) * mmpxl')
    f (r, c) = PixelY $ g $ idx2wpt r c
    g wpt = if is _Just $ findSelectedWall (model ^. walls) wpt 0 then 255 else 0
    mmpxl' = 50
    (w,h) = (1000, 1000)


---------------------
-- Main
---------------------

main :: IO ()
main = do
        args <- getArgs
        maybemodel <- if length args > 0 then decodeFileStrict (args !! 0) else return Nothing
        let model = case maybemodel of
                        Just x -> x
                        Nothing -> defaultModel
        playIO
            FullScreen
            --(InWindow "wallmaker" defaultSSize (0, 0))
            white
            60
            (defaultS model)
            draw
            handleInput
            (\_ s -> return s )

