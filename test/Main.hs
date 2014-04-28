module Main (main) where

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as P
import System.IO (openBinaryFile, IOMode (ReadMode))
import System.Exit (exitFailure)
import Data.Either (isLeft)
import Graphics.ImageSize

assert :: String -> Bool -> IO ()
assert msg x = if x
  then
    putStrLn $ "assert ok: " ++ msg
  else do
    putStrLn $ "assert failed: " ++ msg
    exitFailure

assertEq :: (Show a, Eq a) => a -> a -> IO ()
assertEq x y = do
  assert (show x ++ " == " ++ show y) $ x == y

test :: FilePath -> Maybe (FileFormat, Size) -> IO ()
test path expected = do
  putStrLn $ "# " ++ path
  h <- openBinaryFile path ReadMode
  s <- B.hGet h 200
  let eff = P.parseOnly imageFormatParser s
      esz = P.parseOnly imageSizeParser s
      einfo = P.parseOnly imageInfoParser s

  case expected of
    Just (exff, exsz) -> do
      assertEq eff (Right exff)
      assertEq esz (Right exsz)
    Nothing -> do
      assert ("FileFormat should fail to parse " ++ path) $ isLeft eff
      assert ("Size should fail to parse " ++ path) $ isLeft esz

  case (eff, esz, einfo) of
    (Right ff, Right sz, Right info) -> assertEq (sz, ff) info
    _ -> return ()

  putStrLn ""


main :: IO ()
main = mapM_ (uncurry test)
  [ ("test/images/weather.png", Just (PNG, Size 62 63))
  , ("test/images/cereal.jpg", Just (JPEG, Size 150 112))
  , ("test/images/pulsar.gif", Just (GIF, Size 124 89))
  , ("imagesize.cabal", Nothing)
  ]
