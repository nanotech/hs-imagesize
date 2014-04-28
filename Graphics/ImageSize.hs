{-# LANGUAGE OverloadedStrings
           , TupleSections
           #-}
module Graphics.ImageSize
(
  -- * Parsers
  imageFormatParser
, imageSizeParser
, imageInfoParser
  -- * Types
, Size (..)
, FileFormat (..)
) where

-- Magic number references:
--
-- PNG: http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html (3.1 PNG file signature)
-- GIF: http://www.w3.org/Graphics/GIF/spec-gif89a.txt (17. Header)
--
-- JPEG parsing references:
--
-- http://www.faqs.org/faqs/jpeg-faq/part1/
-- https://gears.googlecode.com/svn/trunk/third_party/libjpeg/rdjpgcom.c
-- http://kd5col.info/swag/GRAPHICS/0143.PAS.html
-- http://hackage.haskell.org/package/imagesize-conduit-1.0.0.2/docs/src/Data-Conduit-ImageSize.html

import Control.Applicative

import Data.ByteString (ByteString)
import Data.Word (Word8, Word16)

import qualified Data.Attoparsec.ByteString as P
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString (Parser)

-- | The pixel dimensions of an image.
data Size = Size { width :: !Int, height :: !Int }
  deriving (Show, Read, Eq, Ord)

-- | The file format of an image.
data FileFormat = GIF | PNG | JPEG
  deriving (Show, Read, Eq, Ord, Enum)

allFileFormats :: [FileFormat]
allFileFormats = enumFrom GIF

formatSignature :: FileFormat -> ByteString
formatSignature ff = case ff of
  PNG  -> "\x89PNG\r\n\x1a\n"
  JPEG -> "\xff\xd8"
  GIF  -> "GIF"


-- | Tries to detect the format of an image by its magic number (header bytes).
imageFormatParser :: Parser FileFormat
imageFormatParser = P.choice $ map
  (\ff -> P.string (formatSignature ff) *> pure ff)
  allFileFormats

-- | Tries to parse the 'Size' of an image. The 'FileFormat' is determined
-- automatically with 'imageFormatParser'.
imageSizeParser :: Parser Size
imageSizeParser = fst <$> imageInfoParser

-- | Combines the output of 'imageSizeParser' and 'imageFormatParser'.
imageInfoParser :: Parser (Size, FileFormat)
imageInfoParser = do
  ff <- imageFormatParser
  (,ff) <$> case ff of
    PNG ->
      P.take 8 -- skip IHDR chunk length and type
      *> sizeParser anyWord32be

    GIF -> do
      P.take 3 -- skip version bytes; 87a or 89a
      *> sizeParser anyWord16le

    JPEG -> parseJPEGSizeSegment


-- JPEG parsing

parseJPEGSegment :: Parser (Word8, Word16)
parseJPEGSegment = do
  P.skipWhile (== 0xFF) -- segment start with optional padding
  segmentType <- P.anyWord8
  segmentLen <- anyWord16be
  return (segmentType, segmentLen - 2) -- subtract because the length includes the length bytes themselves

parseJPEGSizeSegment :: Parser Size
parseJPEGSizeSegment = do
  (segmentType, segmentLen) <- parseJPEGSegment
  if segmentType >= 0xC0 && segmentType <= 0xCF -- SOFn
    then do
      _ <- P.anyWord8 -- skip the data precision field
      Size h w <- sizeParser anyWord16be
      return $ Size w h -- JPEG writes the size backwards compared to other formats
    else do
      _ <- P.take $ fromIntegral segmentLen
      parseJPEGSizeSegment


-- | Parse a pair of integers using the given parser.
sizeParser :: (Applicative f, Integral a) => f a -> f Size
sizeParser int = size <$> int <*> int
  where size w h = Size (fromIntegral w) (fromIntegral h)
