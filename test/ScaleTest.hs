{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Codec.Picture (Image (..), Pixel (..), PixelRGB8 (..), PixelRGBA8 (..), generateImage)
import qualified Codec.Picture.STBIR as STBIR
import Control.Monad (unless)
import Data.Ratio ((%))
import qualified Data.Vector.Storable as V
import System.Exit (exitFailure)

type Cases = forall a. STBIR.STBIRPixel a => String -> Image a -> [String]

main :: IO ()
main = do
  let failures = concatMap (sizeCases halvingCases) halvingSizes ++ concatMap (sizeCases fractionalCases) fractionalSizes
  mapM_ putStrLn failures
  unless (null failures) exitFailure

halvingSizes :: [(Int, Int)]
halvingSizes = [(3000, 2000), (4093, 17), (17, 4093), (1001, 999), (5, 3), (2, 2)]

fractionalSizes :: [(Int, Int)]
fractionalSizes = [(2, 2), (3, 2), (2, 3), (5, 7), (64, 48), (333, 217), (1001, 999), (1920, 1080)]

sizeCases :: Cases -> (Int, Int) -> [String]
sizeCases cases size = cases "rgb" (gradientRGB size) ++ cases "rgba" (gradientRGBA size)

halvingCases :: Cases
halvingCases name img
  | imageWidth img < 2 || imageHeight img < 2 = []
  | otherwise = check name img 2 ++ halvingCases name (downscale img 2)

fractionalCases :: Cases
fractionalCases name img = concatMap (check name img . (% 64)) [65 .. 127]

check :: forall a. STBIR.STBIRPixel a => String -> Image a -> Rational -> [String]
check name img scale = [failure | actual /= (w, h, w * h * componentCount (undefined :: a))]
  where
    out = downscale img (fromRational scale)
    actual = (imageWidth out, imageHeight out, V.length (imageData out))
    w = expected imageWidth
    h = expected imageHeight
    expected f = round $ fromIntegral (f img) / scale
    failure = name <> " " <> show (imageWidth img, imageHeight img) <> " / " <> show scale <> ": " <> show actual

downscale :: STBIR.STBIRPixel a => Image a -> Float -> Image a
downscale img scale = STBIR.resize STBIR.defaultOptions (scaled imageWidth) (scaled imageHeight) img
  where
    scaled f = round $ fromIntegral (f img) / scale

gradientRGB :: (Int, Int) -> Image PixelRGB8
gradientRGB (w, h) = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 7 + y * 3) (fromIntegral $ x * y) (fromIntegral $ x + y * 11)) w h

gradientRGBA :: (Int, Int) -> Image PixelRGBA8
gradientRGBA (w, h) = generateImage (\x y -> PixelRGBA8 (fromIntegral $ x * 7 + y * 3) (fromIntegral $ x * y) (fromIntegral $ x + y * 11) (fromIntegral $ x * 5 + y * 13)) w h
