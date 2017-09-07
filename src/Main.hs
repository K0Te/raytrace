module Main where

import Data.Vector (Vector, (!), fromList)
import Data.Tuple (swap)
import GHC.Word (Word8)
import Codec.Picture
import Codec.Picture.Types

maxX = 640
maxY = 480
-- camera is pointed parallel to z
cameraPos = Point (maxX `div` 2) (maxY * 3 `div` 4) maxX

main :: IO ()
main = writePng "test.png" $ generateImage (\x y -> let (r, g, b) = myPlot ! (x+(maxY-y-1)*maxX) in PixelRGB8 r g b) maxX maxY
  where dumbRender x y = fromList $ draw (Sphere (Point 250 250 400) 200) $ draw Plane emptyPlot
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
data GVector = GVector Int Int Int deriving Show
data Line = Line Point GVector deriving Show

norm :: GVector -> GVector
norm (GVector x y z) = GVector (floor $ (fromIntegral x)*k) (floor $ (fromIntegral y)*k) (floor $ (fromIntegral z)*k)
  where k = 1000.0 / (fromIntegral $ maximum $ fmap abs [x, y, z])

around :: GVector -> GVector -> GVector
-- wrong, definitely not sum :) 2nd vector should be used as ref. coef.
around k1 k2 = GVector (k1x + k2x) (k1y+k2y) (k1z+k2z)
  where (GVector k1x k1y k1z) = norm k1
        (GVector k2x k2y k2z) = norm k2

reflectSphere :: (Int, Int) -> Sphere -> Line
reflectSphere (sx, sy) (Sphere (Point spx spy spz) rad) = Line hitPoint refVec
  where
    (Point cx cy cz) = cameraPos
    distance = sqrt $ fromIntegral $ spx^2 + spy^2 + spz^2
    radReduction = (distance + fromIntegral cz) / (fromIntegral cz)
    projRad = floor $ (fromIntegral rad) / radReduction
    (px, py) = coordToProj radReduction spx spy
    refX = spx + (floor $ (fromIntegral $ sx-px)/radReduction)
    refY = spy + (floor $ (fromIntegral $ sy-py)/radReduction)
    refZ = floor $ sqrt $ fromIntegral $ rad^2 - (spx-refX)^2 - (spy-refY)^2
    maxChange = rad
    hitPoint = Point refX refY refZ
    fallingVec = GVector (sx - cx) (sy-cy) cz
    radVec = GVector (refX - spx) (refY - spy) (refZ - spz)
    refVec = fallingVec `around` radVec

data Plane = Plane
screenPointToPlanePoint :: (Int, Int) -> Maybe Point
screenPointToPlanePoint (sx, sy) =
  let (Point cx cy cz) = cameraPos
      -- plane at y=0 allows to simplify equasion
      steps = (fromIntegral cy) / (fromIntegral $ cy - sy)
      z = (floor $ steps * (fromIntegral cz)) - cz
      x = (floor $ steps * (fromIntegral $ cx - sx)) - cx
  in
  if sy >= cy
  then Nothing -- parallel to plane or diverging
  else Just $ Point x 0 z

data Sphere = Sphere Point Int deriving Show

coordToProj :: Float -> Int -> Int -> (Int, Int)
coordToProj red x y = (scaleX x, scaleY y)
  where xRange = (fromIntegral maxX) / red
        difX = (xRange - (fromIntegral maxX)) / 2.0
        scaleX x = floor $ ((fromIntegral x) + difX) / xRange * (fromIntegral maxX)
        yRange = (fromIntegral maxY) / red
        difY = (yRange - (fromIntegral maxY)) / 2.0
        scaleY y = floor $ ((fromIntegral y) + difY) / yRange * (fromIntegral maxY)

lineToPlanePixel :: Line -> MyPixel
lineToPlanePixel (Line (Point x y z) (GVector dx dy dz)) = if (dy>=0) || isBlack then blackPixel else whitePixel
  where
      -- plane at y=0 allows to simplify equasion
      steps = (fromIntegral y) / (fromIntegral $ y - dy)
      rz = z + (floor $ steps * (fromIntegral dz))
      rx = x + (floor $ steps * (fromIntegral dx))
      whitePixel = (0, 50, 50)
      blackPixel = (0, 0, 0)
      isBlack = mx < squareSize && mz < squareSize || mx >= squareSize && mz >= squareSize
      mx = rx `mod` (squareSize * 2)
      mz = rz `mod` (squareSize * 2)
      squareSize = maxX `div` 5


instance Object Sphere where
  draw (Sphere (Point spx spy spz) rad) plot =
    snd . markSphere <$> enumCoord plot
    where
      markSphere :: ((Int, Int), MyPixel) -> ((Int, Int), MyPixel)
      markSphere (screenPoint, pixel) = (screenPoint, newPixel)
        where newPixel = lineToPixel screenPoint
              hitSphere (sx, sy) = inCircle sx sy
              (Point cx cy cz) = cameraPos
              distance = sqrt $ fromIntegral $ spx^2 + spy^2 + spz^2
              radReduction = (distance + fromIntegral cz) / (fromIntegral cz)
              projRad = floor $ (fromIntegral rad) / radReduction
              (px, py) = coordToProj radReduction spx spy
              inCircle x y = (x-px)^2 + (y-py)^2 <= projRad^2
              refPixel coords sp = (200, refG, refB)
                where line = reflectSphere coords sp
                      (_, refG, refB) = lineToPlanePixel line
              lineToPixel :: (Int, Int) -> MyPixel
              lineToPixel screenPoint = if hitSphere screenPoint then refPixel screenPoint (Sphere (Point spx spy spz) rad) else pixel

instance Object Plane where
  draw plane plot =
    snd . markPlane <$> enumCoord plot
    where
      markPlane :: ((Int, Int), MyPixel) -> ((Int, Int), MyPixel)
      markPlane (screenPoint, pixel) = (screenPoint, newPixel)
        where newPixel = lineToPixel screenPoint Plane
              whitePixel = (200, 200, 200)
              blackPixel = (100, 100, 100)
              isBlack (Point x _ z) = mx < squareSize && mz < squareSize || mx >= squareSize && mz >= squareSize
                where mx = x `mod` (squareSize * 2)
                      mz = z `mod` (squareSize * 2)
                      squareSize = maxX `div` 5
              lineToPixel :: (Int, Int) -> Plane -> MyPixel
              lineToPixel screenPoint _ = case screenPointToPlanePoint screenPoint of Nothing -> pixel
                                                                                      (Just plx) -> if isBlack plx then blackPixel else whitePixel

