module Main where

import Control.Parallel.Strategies (parListChunk, using, rseq)
import Data.Vector ((!), fromList)
import Data.Tuple (swap)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Fixed (mod')
import GHC.Word ()
import Codec.Picture
import Codec.Picture.Types ()

maxX = 1920 :: Int
maxY = 1080 :: Int
refCoef = 0.3
-- camera is pointed parallel to z, so that bottom of the screen is x=[0..maxX], y=0, z=0
cameraPos = intPoint (maxX `div` 2) (maxY `div` 2) (-maxX)

main :: IO ()
main = writePng "test.png" $ generateImage (\x y -> let MyPixel r g b = myPlot ! (x+(maxY-y-1)*maxX) in
                                            PixelRGB8 (floor r) (floor g) (floor b)) maxX maxY
  where scene = [Sphere (intPoint x y x) (fromIntegral r) | x <- (*(2*r)) <$> [0..10], y <- [r]] ++ [Plane]
        r = min (maxX `div` 5) (maxY `div` 5)
        myPlot = fromList $ recursiveRender scene

enumCoord :: [a] -> [((Int, Int), a)]
enumCoord xs = (swap.(toCoord <$>).swap) <$> zip [0..] xs
               where toCoord num = swap $ num `divMod` maxX

data MyPixel = MyPixel !Double !Double !Double
type MyPlot = [MyPixel]
data Point = Point !Double !Double !Double deriving Show
intPoint x y z = Point (fromIntegral x) (fromIntegral y) (fromIntegral z)
data GVector = GVector !Double !Double !Double deriving Show
data Line = Line !Point !GVector deriving Show
data VisibleObject = Plane | Sphere !Point !Double deriving Show
type Ray = (Line, Double, MyPixel)
type Rays = [Ray]

normalize :: GVector -> GVector
normalize (GVector x y z) = GVector (x / len) (y / len) (z / len)
  where len = sqrt $ x^2 + y^2 + z^2

reflect :: GVector -> GVector -> GVector
reflect (GVector k1x k1y k1z) k2 = GVector rx ry rz
  where (GVector nx ny nz) = normalize k2
        kDotn = k1x * nx + k1y * ny + k1z * nz
        refl k n = k - 2*kDotn*n
        rx = refl k1x nx
        ry = refl k1y ny
        rz = refl k1z nz

pixSum :: Double -> MyPixel -> MyPixel -> MyPixel
pixSum k (MyPixel r g b) (MyPixel r2 g2 b2) = MyPixel (kx r r2) (kx g g2) (kx b b2)
  where kx c1 c2 = c1 * (1.0 - k) + k * c2

reflectR :: Ray -> VisibleObject -> Maybe Ray
reflectR (line@(Line _ (GVector x y z)), coef, pixel) Plane = refPixeltoRay <$> hit Plane line
  where refPixeltoRay (Point rx _ rz) = (Line (Point rx 0 rz) (GVector x (-y) z), coef * refCoef, pixSum coef pixel pixel2)
          where pixel2 = if isBlack then MyPixel 0 0 0 else MyPixel 200 200 200
                isBlack = mx < squareSize && mz < squareSize || mx >= squareSize && mz >= squareSize
                mx = rx `mod'` (squareSize * 2)
                mz = rz `mod'` (squareSize * 2)
                -- first row starts with 5 squares
                squareSize = fromIntegral $ maxX `div` 5

reflectR (line@(Line _ vec), coef, pixel) sphere@(Sphere sp _) =
  refPixeltoRay <$> hit sphere line
  where
    refPixeltoRay hitP = (Line hitP refVector, coef * refCoef, pixSum coef pixel (MyPixel 200 20 20))
      where refVector = reflect vec (vecFromPoint sp hitP)

hit :: VisibleObject -> Line -> Maybe Point
hit (Sphere sp rad) (Line point@(Point px py pz) vec) = res
  where
    l@(GVector ddx ddy ddz) = normalize vec
    oc = vecFromPoint sp point :: GVector
    loc = l `dotP` oc :: Double
    dt = loc ** 2 - (oc `dotP` oc) + rad ** 2
    res = if dt <= 0 || dd <= 0 then Nothing else Just $ Point (px + dx) (py + dy) (pz + dz)
      where dd = min (sqrt dt - loc) ((-(sqrt dt)) - loc)
            dx = dd * ddx
            dy = dd * ddy
            dz = dd * ddz
hit Plane (Line (Point x y z) (GVector dx dy dz)) = if dy>=0 then Nothing else Just $ Point rx 0 rz
  where steps =  y / (-dy)
        rz = z + steps * dz
        rx = x + steps * dx

dotP :: GVector -> GVector -> Double
dotP (GVector k1x k1y k1z) k2 = k1x * nx + k1y * ny + k1z * nz
  where (GVector nx ny nz) = k2

vecFromPoint :: Point -> Point -> GVector
vecFromPoint (Point x1 y1 z1) (Point x2 y2 z2) = GVector (x2 - x1) (y2 - y1) (z2 - z1)

distance :: Point -> Point -> Double
distance (Point x1 y1 z1) (Point x2 y2 z2) = sqrt $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

recursiveRender :: [VisibleObject] -> [MyPixel]
recursiveRender scene = (render . rendRay <$> inialRays) `using` parListChunk (maxX `div` 2) rseq
  where
  -- 1. create initial rays
  -- 2. for each one -> check if there possible color change is small enough -> stop
  -- 3. hit each object and use nearest (!) hit point (from ray start)
  -- 4. goto (2)
  emptyPlot = replicate (maxX*maxY) (MyPixel 10 10 10)
  (Point cx cy cz) = cameraPos
  inialRay ((sx,sy), pixel) = (Line (Point cx cy cz) (GVector (fromIntegral sx - cx) (fromIntegral sy - cy) (-cz)), 1.0, pixel)
  inialRays = inialRay <$> enumCoord emptyPlot
  rendRay :: (Line, Double, MyPixel) -> (Line, Double, MyPixel)
  rendRay ray@(Line point _, coef, _) = if coef < 0.01 || null refRays then ray else rendRay $ minimumBy calcDistance refRays
    where refRays = catMaybes $ reflectR ray <$> scene
          calcDistance :: Ray -> Ray -> Ordering
          calcDistance (Line p1 _, _, _) (Line p2 _, _, _) = compare (distance point p1) (distance point p2)
  render :: (Line, Double, MyPixel) -> MyPixel
  render (_, _, pixel) = pixel
