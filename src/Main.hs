module Main where

import Data.Vector (Vector, (!), fromList)
import Data.Tuple (swap)
import Data.List (minimumBy)
import GHC.Word (Word8)
import Codec.Picture
import Codec.Picture.Types
import Debug.Trace

-- TODO
-- Make all zises relative

maxX = 640
maxY = 480
refCoef = 0.5
-- camera is pointed parallel to z
cameraPos = Point (maxX `div` 2) (maxY `div` 2) maxX

main :: IO ()
main = writePng "test.png" $ generateImage (\x y -> let (r, g, b) = myPlot ! (x+(maxY-y-1)*maxX) in PixelRGB8 r g b) maxX maxY
  where scene = [Sphere (Point 300 200 400) 300,
                 Plane]
        -- dumbRender x y = fromList $ foldr draw emptyPlot scene
        dumbRender x y = fromList $ recursiveRender scene
        emptyPlot = replicate (maxX*maxY) (0, 0, 0)
        myPlot = dumbRender maxX maxY

enumCoord :: [a] -> [((Int, Int), a)]
enumCoord [] = []
enumCoord xs = let coords = map toCoord (fst <$> zip [0..] xs)
               in zip coords xs
               where toCoord num = swap $ num `divMod` maxX

type MyPixel = (Word8, Word8, Word8)
type MyPlot =  [MyPixel]
data Point = Point Int Int Int deriving Show
data GVector = GVector Float Float Float deriving Show
intVector x y z = GVector (fromIntegral x) (fromIntegral y) (fromIntegral z)
data Line = Line Point GVector deriving Show

data VisibleObject = Plane | Sphere Point Int deriving Show

vecLen :: GVector -> Float
vecLen (GVector x y z) = sqrt $ x^2 + y^2 + z^2

normalize :: GVector -> GVector
normalize v@(GVector x y z) = let len = vecLen v in GVector (x / len) (y / len) (z / len)

reflect :: GVector -> GVector -> GVector
reflect (GVector k1x k1y k1z) k2 = GVector rx ry rz
  where (GVector nx ny nz) = normalize k2
        kDotn = k1x * nx + k1y * ny + k1z * nz
        refl k n = k - 2*kDotn*n
        rx = refl k1x nx
        ry = refl k1y ny
        rz = refl k1z nz

lineToPlanePixel :: Line -> Maybe (MyPixel, Int, Int)
lineToPlanePixel (Line (Point x y z) (GVector dx dy dz)) = if (dy>=0) then Nothing else if isBlack then Just (blackPixel, rx, rz) else Just (whitePixel, rx, rz)
  where
      -- plane at y=0 allows to simplify equasion
      steps = (fromIntegral y) / dy
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
pixSum k (r, g, b) (r2, g2, b2) = ((r + kx r2), (g + kx g2), (b + kx b2))
  where kx color = floor $ k * fromIntegral color

noPoint = Point 9999 9999 9999

type Ray = (Line, Float, MyPixel)
type Rays = [Ray]
reflectR :: Ray -> VisibleObject -> Ray
reflectR (line@(Line point vv@(GVector x y z)), coef, pixel@(r, g, b)) Plane =
      case lineToPlanePixel line of Nothing -> (Line noPoint vv, 0.0, pixel)
                                    Just (pixel2, rx, rz) -> (Line (Point rx 0 rz) (GVector x y (-z)), coef * refCoef, pixSum coef pixel pixel2)

reflectR (line@(Line point vec@(GVector x y z)), coef, pixel@(r, g, b)) sphere@(Sphere sp@(Point spx spy spz) rad) = (Line hitPoint refVector, resCoef, pixSum coef pixel pixel2)
  where
    hitPointM = hit sphere line
    refVector = case hitPointM of Nothing -> vec
                                  Just hitP -> reflect vec (vecFromPoint sp hitP)
    resCoef = case hitPointM of Nothing -> 0.0
                                Just hitP -> coef * refCoef
    pixel2 = case hitPointM of Nothing -> (0, 0, 0)
                               Just hitP -> (200, 20, 20)
    hitPoint = case hitPointM of Nothing -> noPoint -- TODO use maybe
                                 Just hitP -> hitP

hit :: VisibleObject -> Line -> Maybe Point
hit sphere@(Sphere sp@(Point spx spy spz) rad) line@(Line point@(Point px py pz) vec@(GVector x y z)) = res
  where
    l@(GVector ddx ddy ddz) = normalize vec
    oc = vecFromPoint sp point :: GVector
    loc = l `dotP` oc :: Float
    dt = loc ** 2 - (oc `dotP` oc) + ((fromIntegral rad) ** 2)
    res = if dt <= 0 then Nothing else Just $ Point (px + floor dx) (py + floor dy) (pz + floor dz)
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
recursiveRender scene = render <$> loop scene inialRays
  where
  -- create initial rays
  -- for each one -> check if K is small enough -> early return
  -- hit each object
  -- use nearest (!) hit point (from ray start)
  -- recursion
  emptyPlot = replicate (maxX*maxY) (10, 10, 10)
  (Point cx cy cz) = cameraPos
  inialRay = \((sx,sy), pixel) -> (Line (Point cx cy (-cz)) (GVector (fromIntegral $ sx-cx) (fromIntegral $ sy-cy) (fromIntegral cz)), 1.0, pixel)
  inialRays = inialRay <$> enumCoord emptyPlot
  loop :: [VisibleObject] -> Rays -> Rays
  loop scene rays = rendRay <$> rays
    where rendRay :: (Line, Float, MyPixel) -> (Line, Float, MyPixel)
          rendRay ray@(line@(Line point vec), coef, pixel) = if coef < 0.01 then ray else refRay
            where refRay = rendRay $ minimumBy calcDistance $ (reflectR ray) <$> scene
                  calcDistance :: Ray -> Ray -> Ordering
                  calcDistance (Line p1 v1, _, _) (Line p2 v2, _, _) = compare (distance point p1) (distance point p2)
  render :: (Line, Float, MyPixel) -> MyPixel
  render (_, _, pixel) = pixel
