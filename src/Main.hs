module Main where

import System.Environment (getArgs)
import Codec.Picture
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import System.Random (newStdGen, mkStdGen, random, randomR, randomRIO, getStdGen, Random, RandomGen, randomRs)
import Data.Array.IO
import Data.Queue
-- import Data.Sequence
import qualified Data.Sequence as Seq
import Control.Monad
import Control.Monad.Primitive
import Data.Map (Map)
import qualified Data.Map as Map

-- :set args "/Users/Shirley/Desktop/voronoiArt/img/img1.JPG" "/Users/Shirley/Desktop/voronoiArt/img/img1out.JPG"
-- :set args "/Users/gokcedilek/Desktop/courses/cpsc312/voronoiArt/img/img1.JPG" "/Users/gokcedilek/Desktop/courses/cpsc312/voronoiArt/img/img1-out.JPG"
-- data Point x y () = Point 
data Point = Point Int Int (Int, Int)
      deriving (Show)
instance Eq Point where
  Point x1 y1 _ == Point x2 y2 _ = (x1 == x2 && y1 == y2)

rand :: Int -> IO Int
rand x = do
  r <- randomRIO (1, x)
  return r

{-
*Main> seq1 = Data.Sequence.empty
*Main> seq1 Data.Sequence.|> (Point 3 4 (3,4))
fromList [Point 3 4 (3,4)]
*Main> it Data.Sequence.|> (Point 3 5 (3,5))
fromList [Point 3 4 (3,4),Point 3 5 (3,5)]
-}

genRandomNumbersBetween :: Int -> Int -> (Int, Int) -> [Int]
genRandomNumbersBetween n seed (a, b) = Prelude.take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

generateCentres n inImg = 
  do
    r1 <- rand 100000
    r2 <- rand 100000
    let rs = genRandomNumbersBetween n r1 (0,(imageWidth inImg))
    let ms = genRandomNumbersBetween n r2 (0,(imageHeight inImg))
    -- let rs = genRandomNumbersBetween n r1 (0,50)
    -- let ms = genRandomNumbersBetween n r2 (0,50)
    let both = Prelude.zip rs ms
    let output = [(Point x y (x,y)) | (x,y) <- both]
    return (output)

-- generateValidNeighbours :: Point -> [Point] -> Int -> Int -> [Point]
generateValidNeighbours :: Point -> Map (Int, Int) Bool -> Int -> Int -> [Point]
generateValidNeighbours (Point x y (cx, cy)) processed w h =
  do
    let temp = [(x+1,y), (x-1, y), (x,y+1), (x, y-1)]
    let valid = [Point tx ty (cx, cy) | (tx,ty) <- temp, (isValid tx ty w h processed)]
    -- let temp = [(1,2), (3, 4), (5,6), (7, 8)]
    -- let valid = [Point tx ty (cx, cy) | (tx,ty) <- temp]
    valid

--isValid :: Int -> Int -> Int -> Int -> [Point] -> Boolean
isValid :: Int -> Int -> Int -> Int -> Map (Int, Int) Bool -> Bool
isValid x y w h p
  | (0 <= x && 0 <= y && x <= w && y <= h && not (isProcessed (x,y) p)) = True
  | otherwise = False

isProcessed :: (Int, Int) -> Map (Int, Int) Bool -> Bool
isProcessed (x,y) p
  | (Map.member (x,y) p) = True
  | otherwise = False

pushPoints :: Seq.Seq Point -> [Point] -> Seq.Seq Point
pushPoints q [] = q
pushPoints q (h:t) = pushPoints (q Seq.|> h) t

printPoint :: Point -> (Int, Int, Int, Int)
printPoint (Point x y (cx, cy)) = (x, y, cx, cy)

testQueue :: (Pixel a) => Seq.Seq Point -> Map (Int, Int) Bool -> Image a -> Map (Int, Int) Bool
testQueue q processed inImg =
  case q of
    -- Seq.Empty -> M.unsafeFreezeImage outImg
    Seq.Empty -> processed
    curr Seq.:<| ps -> do
      let (x, y, cx, cy) = printPoint curr
      let isIn = Map.member (x,y) processed
      if (not isIn)
        then do
          
          --let tmp = colorPixelNew curr inImg outImg
          let p = Map.insert (x,y) True processed
          --let nprocessed = curr:processed
          let w = (imageWidth inImg)
          let h = (imageHeight inImg)
          -- let w = 50
          -- let h = 50
          let nbrs = generateValidNeighbours curr p w h
          -- nbrs <- generateValidNeighbours curr nprocessed 3 3
          -- nbrs <- generateValidNeighbours curr nprocessed 70 70
          -- print nbrs
          let nps = pushPoints ps nbrs
          testQueue nps p inImg 
          -- testQueue ps nprocessed inImg
      else
        testQueue ps processed inImg 

voronoiArt inImg centers =
  do
    let queue = Seq.empty
    let newqueue = pushPoints queue centers
    let processed = Map.fromList []
    -- let seq1 = Seq.empty
    -- let seq2 = seq1 Seq.|> (Point 3 4 (3,4))
    -- let seq3 = seq2 Seq.|> (Point 3 5 (3,5))
    -- let p = testQueue seq3 processed inImg
    let p = testQueue newqueue processed inImg
    -- outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
    -- out <- M.unsafeFreezeImage outImg
    -- result <- (savePngImage outPath . ImageYCbCr8) out
    -- let img = writePng outPath $ generateImage (\(Point x y (cx, cy)) -> pixelAt inImg (cx cy)) (imageWidth inImg) (imageHeight inImg)
    -- img <- writePng outPath $ generateImage (\x y -> colorAPixel x y p inImg) (imageWidth inImg) (imageHeight inImg)
    -- let img = generateImage (\x y -> colorAPixel x y p inImg) (imageWidth inImg) (imageHeight inImg)
    -- return img
    -- return output
    return (Map.member (3, 4) p) -- just here for testing purposes

-- find center point of a given (x, y) from the processed points
findPoint (x, y) [] = (x, y)
findPoint (x,y) ((Point xp yp (cx, cy)):t)
  | (x == xp && y == yp) = (cx, cy)
  | otherwise = findPoint (x,y) t

colorAPixel x y p inImg = let (cx, cy) = findPoint (x, y) p in pixelAt inImg cx cy

-- main :: IO ()
main = do
  [inPath, outPath] <- getArgs
  inputImage <- readImage inPath
  case inputImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageYCbCr8 inImg) -> 
    -- Right inImg ->
      do
        returned <- generateCentres 10 inImg
        -- let returned = [Point 1 2 (1,2), Point 0 3 (0,3)]
        -- print returned
        output <- voronoiArt inImg returned
        print output
        --print output
        -- savePngImage outPath output
        -- nbrs <- generateValidNeighbours (Point 3 5 (3,5)) [] 5 5
        -- print nbrs
        putStrLn "HELLO"
        -- return output
  putStrLn "HELLO"

-- unit tests https://hackage.haskell.org/package/HTF (maybe) -- expected results by hand. cannot have randomness.
-- 5*5 7*7 possible center combinations!
-- size of the image, center location, number of centres?
-- centers next to each other
-- centers far apart
-- centers at corners
-- center one of the valid neighbours
-- multiple pixels having all 4 valid nbrs
-- keep everything constant only change one thing at a time

{-
1. center at the very edge: centers=[(0,0)], 5*5
2. centers right next to each other at the same edge: centers=[(0,0), (0,1)] 5*5
3. centers right next to each other in the middle: centers=[(2,3),(3,3)], 5*5
4. centers far apart: 
-}

{-
seq1 = Seq.empty
seq2 = seq1 Seq.|> (Point 3 4 (3,4))
curr = Seq.index seq2 0
seq3 = Seq.deleteAt 0 seq2
-}

-- printQueue :: Seq.Seq Point -> [Point]
-- printQueue q =
--   case q of
--     Seq.Empty -> []
--     p Seq.:<| ps -> do
--       let res = printPoint p
--       let newQueue = Seq.deleteAt 0 q in printQueue newQueue


-- colorPixel :: (Pixel a, PrimMonad m) => Point -> Image a -> M.MutableImage (PrimState m) a -> m ()
-- unsafeFreezeImage :: (Storable (PixelBaseComponent a), PrimMonad m) => MutableImage (PrimState m) a -> m (Image a)Source
--testQueue :: (Pixel a, PrimMonad m) => Seq.Seq Point -> [Point] -> Image a -> M.MutableImage (PrimState m) a  -> ST s (Image a)
-- testQueue q p inImg outImg = 


-- colorPixelNew :: (Pixel a, PrimMonad m) => Point -> Image a -> M.MutableImage (PrimState m) a -> m ()
-- colorPixelNew (Point x y (cx, cy)) inImg outImg = writePixel outImg x y (pixelAt inImg cx cy)

--let tmp = colorPixelNew curr inImg outImg
          -- let (x, y, cx, cy) = printPoint curr
          -- let tmp = writePixel outImg x y (pixelAt inImg cx cy)

-- createImg :: (Control.Monad.Primitive.PrimMonad m) => Image PixelYCbCr8 -> m (Image PixelYCbCr8)
-- createImg inImg = do
--   outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
--   colorPixel 10 100 inImg outImg
--   M.unsafeFreezeImage outImg



-- voronoiArt :: (Monad m, Pixel a) => Image a -> [Point] -> m (Image a)
-- voronoiArt :: (Monad m) => (Image PixelYCbCr8) -> [Point] -> m (Image PixelYCbCr8)


-- imageCreator path = writePng path $ generateImage pixelRenderer 250 300
--    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

--     
-- for (x, y) in p --> find x y in inImg --> writePixel outImg cx cy

-- colorPoints outImg inImg [] = []
-- colorPoints outImg inImg ((Point x y (cx, cy)):t) =
--   do 
--     writePixel outImg x y (pixelAt inImg cx cy)
--     colorPoints outImg inImg t


{-
*Main> seq1 = Seq.empty
*Main> seq2 = seq1 Seq.|> (Point 3 4 (3,4))
*Main> seq3 = seq2 Seq.|> (Point 3 5 (3,5))
*Main> seq3
fromList [Point 3 4 (3,4),Point 3 5 (3,5)]
*Main> printQueue seq3
[Point 3 4 (3,4),Point 3 5 (3,5)]
-}



-- colorPixel :: (Pixel a, PrimMonad m) => Int -> Int -> Image a -> M.MutableImage (PrimState m) a -> m ()
-- colorPixel x y inImg outImg = writePixel outImg x y (pixelAt inImg x y)

-- createImg :: (Control.Monad.Primitive.PrimMonad m) => Image PixelYCbCr8 -> m (Image PixelYCbCr8)
-- createImg inImg = do
--   outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
--   colorPixel 10 100 inImg outImg
--   M.unsafeFreezeImage outImg



-- genRandomNumbersBetween 5 2 (1,10)

-- rand :: Int -> IOArray Int
-- rand x n = do
--   | n == 0 = []
--   | otherwise = 
--   r <- randomRIO (1, x)
--   return r

-- generateCenters :: Int -> Int -> Int -> IO (IOArray (Int, Int))
-- generateCenters _ _ 0 = []
-- generateCenters width height loop = (rand width, rand height):(generateCenters width height (loop-1))

-- generateCenters :: Int -> Int -> Int -> [(IO Int, IO Int)

-- generateCenters _ _ 0 = []
-- generateCenters width height loop = (rand width, rand height):(generateCenters width height (loop-1))

-- genCentres n width height 

-- generateCenters r1 r2 z inImg = [(x,y) | (x,y) <- (zip (genRandomNumbersBetween z r1 (0,(imageWidth inImg))) (genRandomNumbersBetween z r2 (0,(imageHeight inImg))))]

--generateCenters z inImg = [(x,y) | (x,y) <- (zip (genRandomNumbersBetween z r1 (0,(imageWidth inImg))) (genRandomNumbersBetween z r2 (0,(imageHeight inImg))))]

