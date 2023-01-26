#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ ps.JuicyPixels ps.webp ps.JuicyPixels-stbir ])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz

{-# LANGUAGE LambdaCase #-}

import Codec.Picture (DynamicImage (ImageRGBA8), Image (imageHeight, imageWidth), PixelRGBA8, encodePng, readImage)
import Codec.Picture.STBIR (defaultOptions, resize)
import Codec.Picture.WebP (encodeRgba8, encodeRgba8Lossless)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Foreign.C.Types (CFloat)
import Helpers (forShowProgress_)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix (takeBaseName)

type WantedHeight = Int

data Encoding
  = Png
  | LosslessWebP
  | LossyWebP CFloat
  deriving (Show)

main :: IO ()
main = do
  let input = "images"
  let output = "optimized_images"
  let encoding = LossyWebP 30
  let wantedHeight = 256

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

optimizeAndWrite :: Image PixelRGBA8 -> String -> WantedHeight -> Encoding -> IO ()
optimizeAndWrite orig outName wantedHeight encoding =
  let optimize = encode encoding . scale wantedHeight
      optimizedImage = optimize orig
   in B.writeFile (outName <> encodingExtension encoding) optimizedImage

scale :: WantedHeight -> Image PixelRGBA8 -> Image PixelRGBA8
scale wantedHeight img =
  let scaledWidth = imageWidth img * wantedHeight `div` imageHeight img
   in resize defaultOptions scaledWidth wantedHeight img

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
