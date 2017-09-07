module Main where

import Data.Vector (Vector, (!), fromList)
import Data.Tuple (swap)
import GHC.Word (Word8)
import Codec.Picture
import Codec.Picture.Types

-- Where is camera pointed ?
maxX = 640
maxY = 480

main :: IO ()
main = do
  let objects = [ --Line (Point 0 0 0) (Vector 1 2 1)
                --, Line (Point 0 100 0) (Vector 1 (-1) 1)
                Plane]
  let myPlot = dumbRender maxX maxY objects
  let rim = generateImage (\x y -> let (r, g, b) = myPlot ! (x+(maxY-y-1)*maxX) in PixelRGB8 r g b) maxX maxY
  -- \let rim = generateFoldImage  (\acc -> \x -> \y -> (acc+1, PixelRGB8 (acc `mod` 255) ( 255) (255))) 0 640 480
  writePng "test.png" rim
  putStrLn "hello world"

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

data Plane = Plane
cameraPos = Point (maxX `div` 2) (maxY * 3 `div` 4) maxX
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
                      squareSize = 100
              lineToPixel :: (Int, Int) -> Plane -> MyPixel
              lineToPixel screenPoint _ = case screenPointToPlanePoint screenPoint of Nothing -> pixel
                                                                                      (Just plx) -> if isBlack plx then blackPixel else whitePixel

type Render a = Int -> Int -> [a] -> Vector MyPixel

dumbRender :: Object a => Render a
dumbRender x y objects = fromList $ foldr draw emptyPlot objects
  where emptyPlot = replicate (x*y) (0, 0, 0)
