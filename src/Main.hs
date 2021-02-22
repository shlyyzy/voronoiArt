module Main where

import System.Environment (getArgs)
import Codec.Picture
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import System.Random (newStdGen, mkStdGen, random, randomR, randomRIO, getStdGen, Random, RandomGen, randomRs)
import Data.Array.IO
import Data.Queue
import qualified Data.Sequence as Seq
import Control.Monad
import Control.Monad.Primitive
import Data.Map (Map)
import qualified Data.Map as Map

-- Program written by Gokce Dilek and Shirley Yang

-- 1) To run this program, add your initial image to the img/ folder
-- 2) Once you run stack build and stack ghci, set your args like below:
      --  :set args <file_path_in_img> <file_path_out_img> <num_centers>
      --  ie: :set args "/Users/Shirley/Desktop/voronoiArt/img/img1.JPG" "/Users/Shirley/Desktop/voronoiArt/img/img1out.JPG" 100
      --  :set args "/Users/gokcedilek/Desktop/courses/cpsc312/voronoiArt/img/img2.jpg" "/Users/gokcedilek/Desktop/courses/cpsc312/voronoiArt/img/img2-out.jpg" 55
-- 3) Run the program with the `main` command
-- 4) Your out image should now be a new voronoi art!

data Point = Point Int Int (Int, Int)
      deriving (Show)
instance Eq Point where
  Point x1 y1 _ == Point x2 y2 _ = (x1 == x2 && y1 == y2)

-- generate a random number without a seed
rand :: Int -> IO Int
rand x = do
  r <- randomRIO (1, x)
  return r

-- generate a random number with a seed
genRandomNumbersBetween :: Int -> Int -> (Int, Int) -> [Int]
genRandomNumbersBetween n seed (a, b) = Prelude.take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

-- generate n centres based on user input as list of points
generateCentres n inImg = 
  do
    r1 <- rand 100000
    r2 <- rand 100000
    let rs = genRandomNumbersBetween n r1 (0,((imageWidth inImg)-1))
    let ms = genRandomNumbersBetween n r2 (0,((imageHeight inImg)-1))
    let both = Prelude.zip rs ms
    let output = [(Point x y (x,y)) | (x,y) <- both]
    return (output)

-- generate neighbours to point provided they're not processed already or out of scope
generateValidNeighbours :: Point -> Map (Int, Int) (Int, Int) -> Int -> Int -> [Point]
generateValidNeighbours (Point x y (cx, cy)) processed w h =
  do
    let temp = [(x+1,y), (x-1, y), (x,y+1), (x, y-1)]
    let valid = [Point tx ty (cx, cy) | (tx,ty) <- temp, (isValid tx ty w h processed)]
    valid

-- valid is within w and h and not in p
isValid :: Int -> Int -> Int -> Int -> Map (Int, Int) (Int, Int) -> Bool
isValid x y w h p
  | (0 <= x && 0 <= y && x < w && y < h && not (isProcessed (x,y) p)) = True
  | otherwise = False

isProcessed :: (Int, Int) -> Map (Int, Int) (Int, Int) -> Bool
isProcessed (x,y) p
  | (Map.member (x,y) p) = True
  | otherwise = False

-- push list of points to queue
pushPoints :: Seq.Seq Point -> [Point] -> Seq.Seq Point
pushPoints q [] = q
pushPoints q (h:t) = pushPoints (q Seq.|> h) t

printPoint :: Point -> (Int, Int, Int, Int)
printPoint (Point x y (cx, cy)) = (x, y, cx, cy)

-- add all neighbours to centres to a processed queue
testQueue :: (Pixel a) => Seq.Seq Point -> Map (Int, Int) (Int, Int) -> Image a -> Map (Int, Int) (Int, Int)
testQueue q processed inImg =
  case q of
    Seq.Empty -> processed
    curr Seq.:<| ps -> do -- recursive add and process while queue not empty
      let (x, y, cx, cy) = printPoint curr
      let isIn = Map.member (x,y) processed -- only process point if it's not already processed
      if (not isIn)
        then do
          let nprocessed = Map.insert (x, y) (cx, cy) processed
          let w = (imageWidth inImg)
          let h = (imageHeight inImg)
          let nbrs = generateValidNeighbours curr nprocessed w h
          let nps = pushPoints ps nbrs
          testQueue nps nprocessed inImg
      else
        testQueue ps processed inImg 

-- generate points to process
voronoiArt inImg centers =
  do
    let queue = Seq.empty
    let newqueue = pushPoints queue centers
    let processed = Map.fromList []
    let p = testQueue newqueue processed inImg
    return p

findPoint (x, y) p 
  | isProcessed (x, y) p = Map.lookup (x, y) p
  | otherwise = Nothing -- shouldn't happen!

-- color a pixel based on its center point
colorAPixel :: (Pixel a) => (Int, Int) -> Map (Int, Int) (Int, Int) -> Image a -> a
colorAPixel (x, y) p inImg = case findPoint (x, y) p of
  Just (cx, cy) -> pixelAt inImg cx cy
  Nothing -> pixelAt inImg x y -- shouldn't happen!

-- main :: IO ()
main = do
  [inPath, outPath, nc] <- getArgs -- [input img, output img, number of centres]
  inputImage <- readImage inPath
  case inputImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageYCbCr8 inImg) -> 
      do
        returned <- generateCentres (read nc) inImg
        output <- voronoiArt inImg returned      
        let outImg = generateImage (\x y -> colorAPixel (x, y) output inImg) (imageWidth inImg) (imageHeight inImg)
        result <- (savePngImage outPath . ImageYCbCr8) outImg
        putStrLn "Successfully transformed your image!"
  putStrLn "Thanks for using voronoiArt!"
