module ParsePPM where

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.ByteString
import Text.Parsec 
import Text.Parsec.Pos

import Control.Monad
import Data.Word
import Data.Char
import Data.List.Split
import qualified Data.ByteString.Char8 as BS
 
data Pixel = Pixel { red :: Int, green :: Int, blue :: Int
                   } deriving (Show)
data BitMapFormat = BinaryBitMap | TextBitMap
                  deriving (Eq)
data Header = Header { format :: BitMapFormat,
                width :: Int,
                height :: Int,
                depth :: Int
              }
data PPM = PPM { header :: Header,
             bitmap :: [Pixel]
           }

parseInteger :: (Read a, Integral a) => Parser a
parseInteger = fmap read (many1 digit)

parseHeader :: Parser Header
parseHeader = do
  f <- (try (string "P3") >> return TextBitMap) <|> (try (string "P6") >> return BinaryBitMap)
  w <- space >> parseInteger
  h <- space >> parseInteger
  d <- space >> parseInteger

  return (Header f w h d)

parseTextBitMap :: Header -> Parser [Pixel]
parseTextBitMap head = do
  raw <- count (3 * width head * height head) (spaces >> parseInteger)
  return (map (\[a, b, c] -> Pixel a b c) (chunksOf 3 raw))

parseByte :: Parser Int
parseByte = fmap fromEnum anyChar

parseBinaryBitMap :: Header -> Parser [Pixel]
parseBinaryBitMap head = do
  space
  raw <- count (3 * width head * height head) parseByte 
  return (map (\[a, b, c] -> Pixel a b c) (chunksOf 3 raw))

parseBitMap :: Parser PPM
parseBitMap = do
  head <- parseHeader
  let parseBitMap = if format head == TextBitMap then parseTextBitMap else parseBinaryBitMap
  bitmap <- parseBitMap head
  return (PPM head bitmap)

writeBitMap :: FilePath -> PPM -> IO ()
writeBitMap path ppm  = BS.writeFile path (BS.concat [magic, strWidth, strHeight, strDepth, pixData])
  where
    magic = if format (header ppm) == TextBitMap then BS.pack "P3\n" else BS.pack "P6\n"
    strWidth = BS.pack (show (width (header ppm)) ++ " ")
    strHeight = BS.pack (show (height (header ppm)) ++ "\n")
    strDepth = BS.pack (show (depth (header ppm)) ++ "\n")
    pixData = BS.concat (map writePixel (bitmap ppm))
    writePixel = if format (header ppm) == TextBitMap
      then (\pix -> BS.pack ((show (red pix)) ++ " " ++ (show (green pix)) ++ " " ++ (show (blue pix) ++ " ")))
      else (\pix -> BS.pack ([chr (red pix), chr (green pix), chr (blue pix)]))

readBitMap :: FilePath -> IO PPM
readBitMap path = do
  ppm <- BS.readFile path
  either (fail . show) return (parse parseBitMap path ppm)
