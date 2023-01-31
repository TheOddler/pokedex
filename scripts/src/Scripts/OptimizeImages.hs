{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Scripts.OptimizeImages (optimizeImages) where

import Codec.Picture (DynamicImage (..), Image (..), Pixel (..), PixelRGBA8 (..), encodePng, generateImage, readImage)
import Codec.Picture.Extra (crop)
import Codec.Picture.STBIR (defaultOptions, resize)
import Codec.Picture.WebP (encodeRgba8, encodeRgba8Lossless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List (find)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CFloat)
import Helpers (forShowProgress_)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix (takeBaseName)

data Encoding
  = Png
  | LosslessWebP
  | LossyWebP CFloat
  deriving (Show)

optimizeImages :: IO ()
optimizeImages = do
  let input = "images"
  let output = "optimized_images"

  createDirectoryIfMissing True output
  imagePaths <- getDirectoryContents "images"

  forShowProgress_ imagePaths $ \fileName -> do
    errOrImg <- readImage $ input <> "/" <> fileName
    case errOrImg of
      Left err -> putStrLn err
      Right dynImg ->
        case dynImg of
          ImageRGBA8 img -> do
            let baseName = output <> "/" <> takeBaseName fileName

            -- Trying to find the best combination of size and encoding
            -- let optimizeAndWrite' h e = optimizeAndWrite img (baseName <> " - " <> show h <> " - " <> show e) h e

            -- optimizeAndWrite' 128 (LossyWebP 30)
            -- optimizeAndWrite' 128 (LossyWebP 50)
            -- optimizeAndWrite' 128 (LossyWebP 90)
            -- optimizeAndWrite' 128 LosslessWebP

            -- optimizeAndWrite' 256 (LossyWebP 30)
            -- optimizeAndWrite' 256 (LossyWebP 50)
            -- optimizeAndWrite' 256 (LossyWebP 90)
            -- optimizeAndWrite' 256 LosslessWebP

            -- Best solution found
            optimizeAndWrite img baseName 256 (LossyWebP 90)
          _ -> putStrLn "Unsupported type"

optimizeAndWrite :: Image PixelRGBA8 -> String -> Int -> Encoding -> IO ()
optimizeAndWrite orig outName wantedSize encoding =
  let optimize = encode encoding . trimImage . scale wantedSize
      optimizedImage = optimize orig
   in B.writeFile (outName <> encodingExtension encoding) optimizedImage

scale :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
scale wantedSize img@Image {..} =
  let maxDim = max imageWidth imageHeight
      w = imageWidth * wantedSize `div` maxDim
      h = imageHeight * wantedSize `div` maxDim
   in resize defaultOptions w h img

encode :: Encoding -> Image PixelRGBA8 -> ByteString
encode = \case
  Png -> LB.toStrict . encodePng
  LosslessWebP -> encodeRgba8Lossless
  LossyWebP quality -> encodeRgba8 quality

encodingExtension :: Encoding -> String
encodingExtension = \case
  Png -> ".png"
  LosslessWebP -> ".webp"
  LossyWebP _ -> ".webp"

squareImage :: Pixel a => a -> Image a -> Image a
squareImage filler img@Image {..} =
  if imageWidth == imageHeight
    then img
    else generateImage gen size size
  where
    size = max imageWidth imageHeight
    extraWidth = size - imageWidth
    extraHeight = size - imageHeight
    offsetX = extraWidth `div` 2
    offsetY = extraHeight `div` 2
    gen i _ | i < offsetX = filler
    gen i _ | i >= imageWidth + offsetX = filler
    gen _ j | j < offsetY = filler
    gen _ j | j >= imageHeight + offsetY = filler
    gen i j = pixelAt img (i - offsetX) (j - offsetY)

trimImage :: Image PixelRGBA8 -> Image PixelRGBA8
trimImage img@Image {..} = crop left top width height img
  where
    nearlyInvisible :: PixelRGBA8 -> Bool
    nearlyInvisible p = pixelOpacity p == 0
    isInvisibleRow y = all nearlyInvisible $ flip (pixelAt img) y <$> [0 .. imageWidth - 1]
    isInvisibleCol x = all nearlyInvisible $ pixelAt img x <$> [0 .. imageHeight - 1]

    top = fromMaybe imageHeight (find (not . isInvisibleRow) [0 .. imageHeight - 1])
    bottom = fromMaybe 0 (find (not . isInvisibleRow) [imageHeight - 1, imageHeight - 2 .. 0]) + 1
    height = bottom - top - 1

    left = fromMaybe imageWidth (find (not . isInvisibleCol) [0 .. imageWidth - 1])
    right = fromMaybe 0 (find (not . isInvisibleCol) [imageWidth - 1, imageWidth - 2 .. 1]) + 1
    width = right - left - 1
