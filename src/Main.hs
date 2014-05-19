-- ------------------------------------------------
-- Main program to show the GPS location of a jpg
--      image on the Swiss map in the internet browser
-- 
--
-- Read in a jpg file
-- read exif data
-- get GPS Data from exif data
-- convert to Swiss coordinates
-- fireup browser on www.geo.admin.ch
--
-- usage in GHCI: :main "filepatH"
--                :main "/home/roland/Temp/DSC04847.JPG"
--

-- ------------------------------------------------
import Graphics.Hexif
import System.Environment(getArgs)
import Text.Printf(printf)
import Data.Geo.Swiss.Conversion
import Parse
import Control.Applicative
import Hledger.Cli.Utils(openBrowserOn)
import System.Exit(ExitCode)


main :: IO()
main = do
  args <- getArgs
  if length args == 1
       then processFile $ head args
       else putStrLn "give a single filename as parameter"

debug :: IO()
debug = processFile "/home/roland/Temp/RS4847.JPG"


processFile :: String -> IO()
processFile filename = do
   exif <- fromFile filename
   let mbLatt = getTag exif TagGPSLatitude
   let mbLong = getTag exif TagGPSLongitude
   let eLatt = parse "Lattiude" mbLatt
   let eLong = parse "Longitude" mbLong
   let wgs = WGS <$> eLatt <*> eLong
   showResult $ (to03 . wgs2ch) <$> wgs


parse :: String -> Maybe String -> Either String Degree
parse c (Just s)  = getCoord c s
parse c Nothing = Left $ c ++ " No input"

-- | Mark the point in the official Swiss map 
getUrl :: CH03 -> String
getUrl (LV03 x y) = 
    printf "http://map.geo.admin.ch/?&Y=%d&X=%d&zoom=8&crosshair=bowl" x y

-- | Show the result on the Swiss map in your browser
showResult :: Either String CH03 -> IO()
showResult (Left e) = putStrLn e
showResult (Right ch) = do
    openBrowserOn (getUrl ch)
    return ()





