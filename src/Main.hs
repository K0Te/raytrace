module Main where

import Data.Vector (Vector, (!), fromList)
import Data.Tuple (swap)
import GHC.Word (Word8)
import Codec.Picture
import Codec.Picture.Types
import Debug.Trace

maxX = 640
maxY = 480
-- camera is pointed parallel to z
cameraPos = Point (maxX `div` 2) (maxY * 3 `div` 4) maxX

main :: IO ()
main = writePng "test.png" $ generateImage (\x y -> let (r, g, b) = myPlot ! (x+(maxY-y-1)*maxX) in PixelRGB8 r g b) maxX maxY
  where dumbRender x y = fromList $ draw (Sphere (Point 100 300 600) 300) $ draw Plane emptyPlot
        emptyPlot = replicate (maxX*maxY) (0, 0, 0)
        myPlot = dumbRender maxX maxY

enumCoord :: [a] -> [((Int, Int), a)]
enumCoord [] = []
enumCoord xs = let coords = map toCoord (fst <$> zip [0..] xs)
               in zip coords xs
               where toCoord num = swap $ num `divMod` maxX

type MyPixel = (Word8, Word8, Word8)
type MyPlot =  [MyPixel]
class Object a where
  draw :: a -> MyPlot -> MyPlot


data Point = Point Int Int Int deriving Show
data GVector = GVector Float Float Float deriving Show
intVector x y z = GVector (fromIntegral x) (fromIntegral y) (fromIntegral z)
data Line = Line Point GVector deriving Show

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

reflectSphere :: (Int, Int) -> Sphere -> Line
reflectSphere (sx, sy) (Sphere (Point spx spy spz) rad) = Line hitPoint refVec
  where
    (Point cx cy cz) = cameraPos
    distance = sqrt $ fromIntegral $ (spx-cx)^2 + (spy-cy)^2 + (spz+cz)^2
    radReduction = (distance + fromIntegral cz) / (fromIntegral cz)
    projRad = floor $ (fromIntegral rad) / radReduction
    (px, py) = coordToProj radReduction spx spy
    refX = spx + (floor $ (fromIntegral $ sx-px)*radReduction)
    refY = spy + (floor $ (fromIntegral $ sy-py)*radReduction)
    refZ = floor $ sqrt $ fromIntegral $ rad^2 - (spx-refX)^2 - (spy-refY)^2
    maxChange = rad
    hitPoint = Point refX refY refZ
    fallingVec = intVector (sx - cx) (sy-cy) cz
    radVec = intVector ( refX - spx) (refY - spy) (refZ - spz)
    refVec = fallingVec `reflect` radVec

data Plane = Plane
screenPointToPlanePoint :: (Int, Int) -> Maybe MyPixel
screenPointToPlanePoint (sx, sy) =
  -- screen is at z=0
  let (Point cx cy cz) = cameraPos in
  lineToPlanePixel (Line (Point (cx) (cy) (-cz)) (intVector (sx-cx) (sy-cy) cz))

data Sphere = Sphere Point Int deriving Show

coordToProj :: Float -> Int -> Int -> (Int, Int)
coordToProj red x y = (scaleX x, scaleY y)
  where xRange = (fromIntegral maxX) * red
        difX = (xRange - (fromIntegral maxX)) / 2.0
        scaleX x = floor $ ((fromIntegral x) + difX) / xRange * (fromIntegral maxX)
        yRange = (fromIntegral maxY) *  red
        difY = (yRange - (fromIntegral maxY)) / 2.0
        scaleY y = floor $ ((fromIntegral y) + difY) / yRange * (fromIntegral maxY)

lineToPlanePixel :: Line -> Maybe MyPixel
lineToPlanePixel (Line (Point x y z) (GVector dx dy dz)) = if (dy>=0) then Nothing else if isBlack then Just blackPixel else Just whitePixel
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
      squareSize = maxX `div` 5


instance Object Sphere where
  draw sphere@(Sphere (Point spx spy spz) rad) plot =
    snd . markSphere <$> enumCoord plot
    where
      markSphere :: ((Int, Int), MyPixel) -> ((Int, Int), MyPixel)
      markSphere (screenPoint, pixel) = (screenPoint, newPixel)
        where newPixel = lineToPixel screenPoint
              hitSphere (sx, sy) = inCircle sx sy
              (Point cx cy cz) = cameraPos
              distance = sqrt $ fromIntegral $ (spx-cx)^2 + (spy-cy)^2 + (spz+cz)^2
              radReduction = (distance + fromIntegral cz) / (fromIntegral cz)
              projRad = floor $ (fromIntegral rad) / radReduction
              (px, py) = coordToProj radReduction spx spy
              inCircle x y = (x-px)^2 + (y-py)^2 <= projRad^2
              refPixel coords sp = (200, refG, refB)
                where line = reflectSphere coords sp
                      reflectedPixel = lineToPlanePixel line
                      (refB, refG) = case reflectedPixel of Nothing -> (0, 0)
                                                            Just (r, g, b) -> (g `div` 4 * 3, b `div` 4 * 3 )
              lineToPixel :: (Int, Int) -> MyPixel
              lineToPixel screenPoint = if hitSphere screenPoint then refPixel screenPoint sphere else pixel

instance Object Plane where
  draw plane plot =
    snd . markPlane <$> enumCoord plot
    where
      markPlane :: ((Int, Int), MyPixel) -> ((Int, Int), MyPixel)
      markPlane (screenPoint, pixel) = (screenPoint, newPixel)
        where newPixel = lineToPixel screenPoint Plane
              lineToPixel :: (Int, Int) -> Plane -> MyPixel
              lineToPixel screenPoint _ = case screenPointToPlanePoint screenPoint of Nothing -> pixel
                                                                                      (Just plx) -> plx

