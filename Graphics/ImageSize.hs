{-# LANGUAGE OverloadedStrings #-}
module Graphics.ImageSize
( Size (..)
, FileFormat (..)
, detectFileFormat
, parseImageSize
, imageSize
) where

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
import Data.Attoparsec.ByteString (Parser)

data Size = Size { width :: Int, height :: Int }
  deriving (Show, Eq, Ord, Read)

data FileFormat = GIF | PNG | JPEG
  deriving (Show, Eq, Ord, Read, Enum)

justIf :: a -> Bool -> Maybe a
justIf x b = if b then Just x else Nothing

findJust :: [Maybe a] -> Maybe a
findJust = join . find isJust

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

jpgSignature, gifSignature, pngSignature :: ByteString
jpgSignature = "\xff\xd8"
gifSignature = "GIF8"
pngSignature = "\x89PNG\r\n\x1a\n" 

fileFormatMatchers :: [ByteString -> Maybe FileFormat]
fileFormatMatchers =
  [ justIf JPEG . B.isPrefixOf jpgSignature
  , justIf GIF . B.isPrefixOf gifSignature
  , justIf PNG . B.isPrefixOf pngSignature
  ]

-- | Try to detect the format of an image by its magic number (header bytes).
detectFileFormat :: ByteString -> Maybe FileFormat
detectFileFormat s = findJust $ map ($ s) fileFormatMatchers

-- | Parse a pair of integers of a certain width in bytes.
parseSize :: (ByteString -> Int) -> Int -> Parser Size
parseSize readInt n = size <$> P.take n <*> P.take n
  where size w h = Size (readInt w) (readInt h)

-- | A parser that reads the 'Size' of an image with a certain 'FileFormat'.
parseImageSize :: FileFormat -> Maybe (Parser Size)
parseImageSize ff = case ff of
  PNG -> Just $ P.take 16 *> parseSize readBEInt 4
  GIF -> Just $ P.take 6 *> parseSize readLEInt 2
  JPEG -> Just $ parseJPEGSize
  _ -> Nothing


-- JPEG parsing references:
--
-- http://www.faqs.org/faqs/jpeg-faq/part1/
-- https://gears.googlecode.com/svn/trunk/third_party/libjpeg/rdjpgcom.c
-- http://kd5col.info/swag/GRAPHICS/0143.PAS.html
-- http://hackage.haskell.org/package/imagesize-conduit-1.0.0.2/docs/src/Data-Conduit-ImageSize.html

parseJPEGSize :: Parser Size
parseJPEGSize = do
  _ <- P.string jpgSignature -- SOI
  parseJPEGSizeSegment

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
      Size h w <- parseSize readBEInt 2
      return $ Size w h -- JPEG writes the size backwards compared to other formats
    else do
      _ <- P.take segmentLen
      parseJPEGSizeSegment


-- | Read the 'Size' of an image with a certain 'FileFormat'.
imageSize :: FileFormat -> ByteString -> Maybe Size
imageSize ff s = parseImageSize ff >>= \parser -> hush (P.parseOnly parser s)

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
  let mff = detectFileFormat s
  print mff
  print $ mff >>= (`imageSize` s)
  putStrLn ""

main :: IO ()
main = mapM_ test
  [ "test-images/weather.png"
  , "test-images/cereal.jpg"
  , "test-images/pulsar.gif"
  ]
