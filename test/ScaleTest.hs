{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Codec.Picture (Image (..), Pixel (..), PixelRGB8 (..), PixelRGBA8 (..), generateImage)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (pixelFold)
import Control.Monad (unless)
import Data.Bits (xor)
import Data.Ratio ((%))
import qualified Data.Vector.Storable as V
import Data.Word (Word64)
import System.Exit (exitFailure)

type Cases = forall a. STBIR.STBIRPixel a => String -> Image a -> [String]

main :: IO ()
main = do
  let failures = concatMap (sizeCases halvingCases) halvingSizes ++ concatMap (sizeCases fractionalCases) fractionalSizes ++ pinnedCatmullRom
  mapM_ putStrLn failures
  unless (null failures) exitFailure

halvingSizes :: [(Int, Int)]
halvingSizes = [(3000, 2000), (4093, 17), (17, 4093), (1001, 999), (5, 3), (2, 2)]

fractionalSizes :: [(Int, Int)]
fractionalSizes = [(2, 2), (3, 2), (2, 3), (5, 7), (64, 48), (333, 217), (1001, 999), (1920, 1080)]

sizeCases :: Cases -> (Int, Int) -> [String]
sizeCases cases size = cases "rgb" (solid (PixelRGB8 200 100 50) size) ++ cases "rgba" (solid (PixelRGBA8 200 100 50 128) size)

halvingCases :: Cases
halvingCases name img
  | imageWidth img < 2 || imageHeight img < 2 = []
  | otherwise = check name img 2 ++ halvingCases name (downscale img 2)

fractionalCases :: Cases
fractionalCases name img = concatMap (check name img . (% 64)) [65 .. 127]

check :: STBIR.STBIRPixel a => String -> Image a -> Rational -> [String]
check name img scale = [failure | changed > 0]
  where
    colour = pixelAt img 0 0
    changed = pixelFold (\n _ _ px -> if px == colour then n else n + 1) (0 :: Int) (downscale img (fromRational scale))
    failure = name <> " " <> show (imageWidth img, imageHeight img) <> " / " <> show scale <> ": " <> show changed <> " pixels changed"

pinnedCatmullRom :: [String]
pinnedCatmullRom = [failure | actual /= 14333637002857317752]
  where
    out = STBIR.resize options 320 240 (gradientRGB (1001, 999))
    options = STBIR.defaultOptions {STBIR.filterHorizontal = STBIR.FILTER_CATMULLROM, STBIR.filterVertical = STBIR.FILTER_CATMULLROM}
    actual = V.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64) (imageData out)
    failure = "catmull-rom rgb (1001,999) -> (320,240): " <> show actual

downscale :: STBIR.STBIRPixel a => Image a -> Float -> Image a
downscale img scale = STBIR.resize STBIR.defaultOptions (scaled imageWidth) (scaled imageHeight) img
  where
    scaled f = round $ fromIntegral (f img) / scale

gradientRGB :: (Int, Int) -> Image PixelRGB8
gradientRGB (w, h) = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 7 + y * 3) (fromIntegral $ x * y) (fromIntegral $ x + y * 11)) w h

solid :: Pixel a => a -> (Int, Int) -> Image a
solid colour (w, h) = generateImage (\_ _ -> colour) w h
