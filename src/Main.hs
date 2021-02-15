module Main where

import System.Environment (getArgs)
import Codec.Picture
import qualified Codec.Picture.Types as M
import System.Random (newStdGen, mkStdGen, random, randomR, randomRIO, getStdGen, Random, RandomGen, randomRs)
import Data.Array.IO

-- :set args "/Users/Shirley/Desktop/voronoiArt/img/img1.JPG"

rand :: Int -> IO Int
rand x = do
  r <- randomRIO (1, x)
  return r


genRandomNumbersBetween :: Int -> Int -> (Int, Int) -> [Int]
genRandomNumbersBetween n seed (a, b) = take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed
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


generateCentres n inImg = 
  do
    r1 <- rand 100000
    r2 <- rand 100000
    let rs = genRandomNumbersBetween n r1 (0,(imageWidth inImg))
    let ms = genRandomNumbersBetween n r2 (0,(imageHeight inImg)) 
    return (zip rs ms)

main :: IO ()
main = do
  [inPath] <- getArgs
  inputImage <- readImage inPath
  case inputImage of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageYCbCr8 inImg) -> 
      do
        returned <- genCentres 5 inImg
        print(returned)
        putStrLn "HELLO"
  putStrLn "HELLO"
  
{-
main :: IO()
main = do
  [inPath, outPath] <- getArgs
  inputImg <- readImage inPath
  case inputImg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 inImg) ->
      (savePngImage outPath . ImageRGB8 . copyImg) inImg
    Right _ -> putStrLn "Unexpected pixel format"

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