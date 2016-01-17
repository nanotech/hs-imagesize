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
, JPEGContainer (..)
, Endianness (..)
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
--
-- TIFF parsing references:
--
-- http://www.asmail.be/msg0055375048.html
-- http://www.media.mit.edu/pia/Research/deepview/exif.html
-- http://www.awaresystems.be/imaging/tiff/tifftags/baseline.html

import Control.Applicative
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Debug.Trace

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.Attoparsec.ByteString as P
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString (Parser)

-- | The pixel dimensions of an image.
data Size = Size { width :: !Int, height :: !Int }
  deriving (Show, Read, Eq, Ord)

-- | The file format of an image.
data FileFormat = GIF | PNG | JPEG JPEGContainer | TIFF Endianness
  deriving (Show, Read, Eq, Ord)

data JPEGContainer = JFIF | Exif Endianness
  deriving (Show, Read, Eq, Ord)

data Endianness = Big | Little
  deriving (Show, Read, Eq, Ord, Enum)

allFileFormats :: [FileFormat]
allFileFormats =
  [ GIF
  , PNG
  , JPEG JFIF
  , JPEG (Exif Big)
  , JPEG (Exif Little)
  , TIFF Big
  , TIFF Little
  ]

formatSignature :: FileFormat -> ByteString
formatSignature ff = case ff of
  PNG  -> "\x89PNG\r\n\x1a\n"
  JPEG JFIF -> "\xff\xd8\xff\xe0"
  JPEG (Exif _) -> "\xff\xd8\xff\xe1"
  GIF  -> "GIF"
  TIFF Little -> "II\42\0"
  TIFF Big -> "MM\0\42"

signatureParser :: FileFormat -> Parser FileFormat
signatureParser ff = case ff of
  JPEG (Exif en) ->
       P.string (formatSignature ff)
    *> P.take 2 -- FIXME: What is this?
    *> P.string "Exif"
    *> P.take 2 -- FIXME: What is this?
    *> P.string (formatSignature (TIFF en))
    *> pure ff

  _ -> P.string (formatSignature ff) *> pure ff


-- | Tries to detect the format of an image by its magic number (header bytes).
imageFormatParser :: Parser FileFormat
imageFormatParser = P.choice $ map signatureParser allFileFormats

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

    GIF ->
      P.take 3 -- skip version bytes; 87a or 89a
      *> sizeParser anyWord16le

    TIFF en -> parseTIFF 4 en

    JPEG JFIF -> do
      -- FFE1, the segment padding and type, was
      -- already parsed as part of the header
      segmentLen <- anyWord16be
      _ <- P.string "JFIF"
      _ <- P.take $ fromIntegral segmentLen - 6 -- subtract segment length bytes + JFIF string
      parseJPEGSizeSegment

    JPEG (Exif en) ->
      -- _ <- P.take 2 -- FIXME: What is this?
      -- _ <- P.string "Exif"
      -- _ <- P.take 2 -- FIXME: What is this?
      -- _ <- P.string (formatSignature (TIFF en))
      parseTIFF 4 en

-- JPEG parsing

parseJPEGSegment :: Parser (Word8, Word16)
parseJPEGSegment = do
  P.skipWhile (== 0xFF) -- segment start with optional padding
  segmentType <- P.anyWord8
  segmentLen <- anyWord16be
  --traceShowM (segmentType, segmentLen)
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


-- TIFF parsing

parseTIFF :: Int -> Endianness -> Parser Size
parseTIFF currentOffset en = do
  firstImageOffset <- fromIntegral <$> anyWord32 en -- an absolute offset into the file
  let currentOffset' = currentOffset + 4
  --traceShowM firstImageOffset
  _ <- P.take $ firstImageOffset - currentOffset'
  let currentOffset'' = firstImageOffset
  --traceShowM (currentOffset'', currentOffset')
  allTags <- parseTIFFIFD en
  let currentOffset''' = currentOffset'' + 2 + length allTags * tiffTagLength
  let tags = catMaybes allTags
  let mSize = findSizeTags tiffTagImageWidth tiffTagImageLength tags
  case mSize of
    Just size -> return size
    Nothing -> do
      let mOffset = lookup tiffTagExifOffset tags
      case mOffset of
        Just offset -> do
          _ <- P.take $ fromIntegral offset - currentOffset'''
          allExifTags <- parseTIFFIFD en
          let exifTags = catMaybes allExifTags
          --traceShowM (offset, offset - (currentOffset + 6 + length allTags * tiffTagLength))
          --traceShowM (length allTags, exifTags)
          maybe kaboom return $ findSizeTags exifTagImageWidth exifTagImageHeight exifTags
        Nothing -> kaboom

  where kaboom = fail "One or both of the ImageWidth and ImageLength TIFF tags were not found"

findSizeTags :: Eq a => a -> a -> [(a, Int)] -> Maybe Size
findSizeTags wt ht tags = Size <$> lookup wt tags <*> lookup ht tags

parseTIFFIFD :: Endianness -> Parser [Maybe (Word16, Int)]
parseTIFFIFD en = do
  tagCount <- anyWord16 en
  --traceShowM tagCount
  replicateM (fromIntegral tagCount) (parseTIFFTag en)

tiffTagImageWidth, tiffTagImageLength, tiffTagExifOffset :: Word16
tiffTagImageWidth = 256
tiffTagImageLength = 257 -- height, not byte length
tiffTagExifOffset = 0x8769 -- = 34665

exifTagImageWidth, exifTagImageHeight :: Word16
exifTagImageWidth = 0xa002
exifTagImageHeight = 0xa003

tiffTagLength :: Int
tiffTagLength = 12

knownTIFFTags :: [Word16]
knownTIFFTags =
 [ tiffTagImageWidth
 , tiffTagImageLength
 , tiffTagExifOffset
 , exifTagImageWidth
 , exifTagImageHeight
 ]

parseTIFFTag :: Endianness -> Parser (Maybe (Word16, Int))
parseTIFFTag en = do
  tagID <- anyWord16 en
  --let currentOffset = 10
  --let bytesRead = currentOffset + tagIndex * tiffTagLength
  --traceShowM (tagIndex, bytesRead, "id" :: String, tagID)
  --traceShowM ("id" :: String, tagID)
  if tagID `elem` knownTIFFTags
    then do
      mTagValue <- parseTIFFTagValue en
      return $ (tagID,) <$> mTagValue
    else
      P.take 10 -- tags are always 12 bytes. skip the remainder.
      *> pure Nothing

parseTIFFTagValue :: Endianness -> Parser (Maybe Int)
parseTIFFTagValue en = do
  tagType <- anyWord16 en
  --traceShowM ("type" :: String, tagType)
  _ <- anyWord32 en -- value count; always 1 for these tags
  mTagValue <- case tagType of
    3 -> Just . fromIntegral <$> anyWord16 en <* P.take 2
    4 -> Just . fromIntegral <$> anyWord32 en
    _ -> return Nothing
  --traceShowM ("val" :: String, mTagValue)
  return mTagValue


-- Endian-parameterized parsing

anyWord16 :: Endianness -> Parser Word16
anyWord16 Big    = anyWord16be
anyWord16 Little = anyWord16le

anyWord32 :: Endianness -> Parser Word32
anyWord32 Big    = anyWord32be
anyWord32 Little = anyWord32le


-- | Parse a pair of integers using the given parser.
sizeParser :: (Applicative f, Integral a) => f a -> f Size
sizeParser int = size <$> int <*> int
  where size w h = Size (fromIntegral w) (fromIntegral h)
