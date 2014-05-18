-- -----------------------------------------
-- Parse.hs
-- -----------------------------------------
-- Try to parse strings like "46, 28, 0.29" or "  8,  9, 59.98"
--
-- ----------------------------------------
module Parse (getCoord) where

import Text.Parsec
import Text.Parsec.String
import Data.Geo.Swiss.Conversion


comma :: Parser Char
comma = char ','

digitOrDot :: Parser Char
digitOrDot = digit <|> char '.'

parseInt :: Parser Int
parseInt = read `fmap` many1 digit

parseFloat :: Parser Double
parseFloat = read `fmap` many1 digitOrDot

parseCoord :: Parser Degree
parseCoord = do
    spaces 
    d <- parseInt
    _ <- comma
    spaces
    m <- parseInt
    _ <- comma
    spaces
    s <- parseFloat
    return $ Deg d m s

getCoord :: String -> String -> Either String Degree
getCoord c s = case parse parseCoord c s of
    Left e      ->  Left $ show e
    Right coord ->  Right coord
