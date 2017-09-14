module Main where

import Control.Parallel.Strategies (parListChunk, using, rseq)
import Data.Vector (Vector, (!), fromList)
import Data.Tuple (swap)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import GHC.Word (Word8)
import Codec.Picture
import Codec.Picture.Types
import Debug.Trace

maxX = 1920
maxY = 1080
refCoef = 0.3
-- camera is pointed parallel to z, so that bottom of the screen is x=[0..maxX], y=0, z=0
cameraPos = Point (maxX `div` 2) (maxY `div` 2) (-maxX)

main :: IO ()
main = writePng "test.png" $ generateImage (\x y -> let (r, g, b) = myPlot ! (x+(maxY-y-1)*maxX) in PixelRGB8 r g b) maxX maxY
  where scene = [Sphere (Point (maxX `div` 5) (maxY `div` 3) (maxY `div` 2)) (maxY `div` 3),
                 Sphere (Point maxX (maxY `div` 3) (maxY*2)) (maxY `div` 2),
                 Plane]
        myPlot = fromList $ recursiveRender scene

enumCoord :: [a] -> [((Int, Int), a)]
enumCoord xs = (swap.(toCoord <$>).swap) <$> (zip [0..] xs)
               where toCoord num = swap $ num `divMod` maxX

type MyPixel = (Word8, Word8, Word8)
type MyPlot = [MyPixel]
data Point = Point !Int !Int !Int deriving Show
data GVector = GVector !Float !Float !Float deriving Show
data Line = Line !Point !GVector deriving Show
data VisibleObject = Plane | Sphere !Point !Int deriving Show
type Ray = (Line, Float, MyPixel)
type Rays = [Ray]

normalize :: GVector -> GVector
normalize v@(GVector x y z) = GVector (x / len) (y / len) (z / len)
  where len = sqrt $ x^2 + y^2 + z^2

reflect :: GVector -> GVector -> GVector
reflect (GVector k1x k1y k1z) k2 = GVector rx ry rz
  where (GVector nx ny nz) = normalize k2
        kDotn = k1x * nx + k1y * ny + k1z * nz
        refl k n = k - 2*kDotn*n
        rx = refl k1x nx
        ry = refl k1y ny
        rz = refl k1z nz

-- TODO rewrite this and "hit" as one function
lineToPlanePixel :: Line -> Maybe (MyPixel, Int, Int)
lineToPlanePixel (Line (Point x y z) (GVector dx dy dz)) = if (dy>=0) then Nothing else if isBlack then Just (blackPixel, rx, rz) else Just (whitePixel, rx, rz)
  where
      -- plane at y=0 allows to simplify equasion
      steps = (fromIntegral y) / (-dy)
      rz = z + (floor $ steps * dz)
      rx = x + (floor $ steps * dx)
      whitePixel = (200, 200, 200)
      blackPixel = (0, 0, 0)
      isBlack = mx < squareSize && mz < squareSize || mx >= squareSize && mz >= squareSize
      mx = rx `mod` (squareSize * 2)
      mz = rz `mod` (squareSize * 2)
      -- first row starts with 5 squares
      squareSize = maxX `div` 5

pixSum :: Float -> MyPixel -> MyPixel -> MyPixel
pixSum k (r, g, b) (r2, g2, b2) = ((kx r r2), (kx g g2), (kx b b2))
  where kx c1 c2 = floor $ (fromIntegral c1) * (1.0 - k) + k * fromIntegral c2

reflectR :: Ray -> VisibleObject -> Maybe Ray
reflectR (line@(Line point vv@(GVector x y z)), coef, pixel@(r, g, b)) Plane = refPixeltoRay <$> lineToPlanePixel line
  where refPixeltoRay (pixel2, rx, rz) = (Line (Point rx 0 rz) (GVector x (-y) z), coef * refCoef, pixSum coef pixel pixel2)

reflectR (line@(Line point vec@(GVector x y z)), coef, pixel@(r, g, b)) sphere@(Sphere sp@(Point spx spy spz) rad) =
  refPixeltoRay <$> hit sphere line
  where
    refPixeltoRay hitP = (Line hitP refVector, coef * refCoef, pixSum coef pixel (200, 20, 20))
      where refVector = reflect vec (vecFromPoint sp hitP)

hit :: VisibleObject -> Line -> Maybe Point
hit (Sphere sp@(Point spx spy spz) rad) (Line point@(Point px py pz) vec@(GVector x y z)) = res
  where
    l@(GVector ddx ddy ddz) = normalize vec
    oc = vecFromPoint sp point :: GVector
    loc = l `dotP` oc :: Float
    dt = loc ** 2 - (oc `dotP` oc) + ((fromIntegral rad) ** 2)
    res = if dt <= 0 || dd <= 0 then Nothing else Just $ Point (px + floor dx) (py + floor dy) (pz + floor dz)
      where dd = min ((sqrt dt) - loc) ((-(sqrt dt)) - loc)
            dx = dd * ddx
            dy = dd * ddy
            dz = dd * ddz

dotP :: GVector -> GVector -> Float
dotP (GVector k1x k1y k1z) k2 = k1x * nx + k1y * ny + k1z * nz
  where (GVector nx ny nz) = k2

vecFromPoint :: Point -> Point -> GVector
vecFromPoint (Point x1 y1 z1) (Point x2 y2 z2) = GVector (fromIntegral $ x2 - x1) (fromIntegral $ y2 - y1) (fromIntegral $ z2 - z1)

distance :: Point -> Point -> Float
distance (Point x1 y1 z1) (Point x2 y2 z2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2


recursiveRender :: [VisibleObject] -> [MyPixel]
recursiveRender scene = render <$> rendRay <$> inialRays `using` parListChunk maxX rseq
  where
  -- 1. create initial rays
  -- 2. for each one -> check if there possible color change is small enough -> stop
  -- 3. hit each object and use nearest (!) hit point (from ray start)
  -- 4. goto (2)
  emptyPlot = replicate (maxX*maxY) (10, 10, 10)
  (Point cx cy cz) = cameraPos
  inialRay = \((sx,sy), pixel) -> (Line (Point cx cy cz) (GVector (fromIntegral $ sx-cx) (fromIntegral $ sy-cy) (fromIntegral (-cz))), 1.0, pixel)
  inialRays = inialRay <$> enumCoord emptyPlot
  rendRay :: (Line, Float, MyPixel) -> (Line, Float, MyPixel)
  rendRay ray@((Line point vec), coef, pixel) = if coef < 0.01 || null refRays then ray else rendRay $ minimumBy calcDistance $ refRays
    where refRays = catMaybes $ (reflectR ray) <$> scene
          calcDistance :: Ray -> Ray -> Ordering
          calcDistance (Line p1 v1, _, _) (Line p2 v2, _, _) = compare (distance point p1) (distance point p2)
  render :: (Line, Float, MyPixel) -> MyPixel
  render (_, _, pixel) = pixel
