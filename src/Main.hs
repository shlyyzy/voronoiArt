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

-- :set args "/Users/Shirley/Desktop/voronoiArt/img/img1.JPG"
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
    let both = Prelude.zip rs ms
    let output = [(Point x y (x,y)) | (x,y) <- both]
    return (output)

-- generateValidNeighbours :: Point -> [Point] -> Int -> Int -> [Point]
generateValidNeighbours (Point x y (cx, cy)) processed w h =
  do
    -- might be best to test if point is valid before generating
      -- add to processed?
    let temp = [(x+1,y), (x-1, y), (x,y+1), (x, y-1)]
    let valid = [Point tx ty (cx, cy) | (tx,ty) <- temp, (isValid tx ty w h processed)]
    -- let temp = [(1,2), (3, 4), (5,6), (7, 8)]
    -- let valid = [Point tx ty (cx, cy) | (tx,ty) <- temp]
    return (valid)

--isValid :: Int -> Int -> Int -> Int -> [Point] -> Boolean
isValid x y w h p
  | (0 <= x && 0 <= y && x <= w && y <= h && not (isProcessed (x,y) p)) = True
  | otherwise = False

isProcessed _ [] = False
isProcessed (x,y) ((Point xp yp (cx, cy)):t)
  | (x == xp && y == yp) = True
  | otherwise = isProcessed (x,y) t


main :: IO ()
main = do
  [inPath, outPath] <- getArgs
  inputImage <- readImage inPath
  case inputImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageYCbCr8 inImg) -> 
      do
        returned <- generateCentres 5 inImg
        output <- voronoiArt inImg returned outPath
        putStrLn "HELLO"
  putStrLn "HELLO"

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
testQueue :: (Pixel a) => Seq.Seq Point -> [Point] -> Image a -> [Point]
testQueue q p inImg =
  case q of
    -- Seq.Empty -> M.unsafeFreezeImage outImg
    Seq.Empty -> p
    curr Seq.:<| ps -> do
      -- let res = printPoint p
      if (not (elem curr p))
        then do
          let p = curr:p
          --let tmp = colorPixelNew curr inImg outImg
          let (x, y, cx, cy) = printPoint curr
          -- let tmp = writePixel outImg x y (pixelAt inImg cx cy)
          nbrs <- generateValidNeighbours curr p (imageWidth inImg) (imageHeight inImg)
          let ps = pushPoints ps nbrs
          testQueue ps p inImg 
      else
        testQueue ps p inImg 

{-
copyImg :: Image PixelRGB8 -> Image PixelRGB8
copyImg inImg@Image {..} = runST $ do -- private piazza ? {..}
  out <- M.newMutableImage imageWidth imageHeight
  let colorPixel x y
        | x >= imageWidth  = colorPixel 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage out
        | otherwise = do
            writePixel out x y (pixelAt inImg x y)
            colorPixel (x + 1) y
  colorPixel 0 0
-}

-- copyImg :: Image PixelRGB8 -> Image PixelRGB8
-- copyImg inImg@Image {..} = runST $ do -- private piazza ? {..}
--   out <- M.newMutableImage imageWidth imageHeight
--   let colorPixel x y
--         | x >= imageWidth  = colorPixel 0 (y + 1)
--         | y >= imageHeight = M.unsafeFreezeImage out
--         | otherwise = do
--             writePixel out x y (pixelAt inImg x y)
--             colorPixel (x + 1) y
--   colorPixel 0 0

-- colorPixelNew :: (Pixel a, PrimMonad m) => Point -> Image a -> M.MutableImage (PrimState m) a -> m ()
colorPixelNew (Point x y (cx, cy)) inImg outImg = writePixel outImg x y (pixelAt inImg cx cy)

-- createImg :: (Control.Monad.Primitive.PrimMonad m) => Image PixelYCbCr8 -> m (Image PixelYCbCr8)
-- createImg inImg = do
--   outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
--   colorPixel 10 100 inImg outImg
--   M.unsafeFreezeImage outImg

--voronoiArt :: Image PixelYCbCr8 -> [Point] -> [Point]
voronoiArt inImg centers outPath =
  do
    let queue = Seq.empty
    let queue = pushPoints queue centers
    let processed = []
    let p = testQueue queue processed inImg
    -- outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
    let a = colorPoints outImg inImg p
    -- out <- M.unsafeFreezeImage outImg
    -- result <- (savePngImage outPath . ImageYCbCr8) out
    let img = writePng outPath $ generateImage (\(Point x y (cx, cy)) -> pixelAt inImg (cx cy)) (imageWidth inImg) (imageHeight inImg)
    return (result)

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
args <- getArgs
case length args of
    0 -> putStrLn "No Arguments, exiting"
    otherwise -> do
        other
        methods
        here
-}


{-
*Main> seq1 = Seq.empty
*Main> seq2 = seq1 Seq.|> (Point 3 4 (3,4))
*Main> seq3 = seq2 Seq.|> (Point 3 5 (3,5))
*Main> seq3
fromList [Point 3 4 (3,4),Point 3 5 (3,5)]
*Main> printQueue seq3
[Point 3 4 (3,4),Point 3 5 (3,5)]
-}

printPoint :: Point -> (Int, Int, Int, Int)
printPoint (Point x y (cx, cy)) = (x, y, cx, cy)

colorPixel :: (Pixel a, PrimMonad m) => Int -> Int -> Image a -> M.MutableImage (PrimState m) a -> m ()
colorPixel x y inImg outImg = writePixel outImg x y (pixelAt inImg x y)



-- createImg :: (Control.Monad.Primitive.PrimMonad m) => Image PixelYCbCr8 -> m (Image PixelYCbCr8)
-- createImg inImg = do
--   outImg <- M.newMutableImage (imageWidth inImg) (imageHeight inImg)
--   colorPixel 10 100 inImg outImg
--   M.unsafeFreezeImage outImg

pushPoints :: Seq.Seq Point -> [Point] -> Seq.Seq Point
pushPoints q [] = q
pushPoints q (h:t) = pushPoints (q Seq.|> h) t

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

