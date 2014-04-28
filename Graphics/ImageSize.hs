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

import Data.Maybe
import Data.List (find)
import Control.Applicative
import Control.Monad (join)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Bits (shiftL)
import Data.Word (Word8)
import System.IO (openBinaryFile, IOMode (ReadMode))

import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.Combinator as P
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
      *> sizeParser readBEInt 4

    GIF -> do
      P.take 3 -- skip version bytes; 87a or 89a
      *> sizeParser readLEInt 2

    JPEG -> parseJPEGSizeSegment


-- JPEG parsing

parseJPEGSegment :: Parser (Word8, Int)
parseJPEGSegment = do
  P.skipWhile (== 0xFF) -- segment start with optional padding
  segmentType <- P.anyWord8
  segmentLen <- readBEInt <$> P.take 2
  return (segmentType, segmentLen - 2) -- subtract because the length includes the length bytes themselves

parseJPEGSizeSegment :: Parser Size
parseJPEGSizeSegment = do
  (segmentType, segmentLen) <- parseJPEGSegment
  if segmentType >= 0xC0 && segmentType <= 0xCF -- SOFn
    then do
      _ <- P.anyWord8 -- skip the data precision field
      Size h w <- sizeParser readBEInt 2
      return $ Size w h -- JPEG writes the size backwards compared to other formats
    else do
      _ <- P.take segmentLen
      parseJPEGSizeSegment


-- | Parse a pair of integers of a certain width in bytes.
sizeParser :: (ByteString -> Int) -> Int -> Parser Size
sizeParser readInt n = size <$> P.take n <*> P.take n
  where size w h = Size (readInt w) (readInt h)

-- | Interpret a 'ByteString' as an unsigned big-endian integer.
readBEInt :: ByteString -> Int
readBEInt = readSomeInt B.foldr

-- | Interpret a 'ByteString' as an unsigned little-endian integer.
readLEInt :: ByteString -> Int
readLEInt = readSomeInt $ B.foldl . flip

readSomeInt :: Integral byte => ((byte -> (Int, Int) -> (Int, Int)) -> (Int, Int) -> a -> (c, b)) -> a -> c
readSomeInt fold = fst . fold (\c (a,i) -> (a + (fromIntegral c `shiftL` i), i + 8)) (0,0)


test :: String -> IO ()
test path = do
  h <- openBinaryFile path ReadMode
  print h
  s <- B.hGet h 200
  print $ P.parseOnly imageFormatParser s
  print $ P.parseOnly imageSizeParser s
  putStrLn ""

main :: IO ()
main = mapM_ test
  [ "test-images/weather.png"
  , "test-images/cereal.jpg"
  , "test-images/pulsar.gif"
  , "imagesize.cabal"
  ]
