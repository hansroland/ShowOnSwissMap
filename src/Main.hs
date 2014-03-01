-- ------------------------------------------------
-- Main program to show the GPS location of an jpg
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
import Graphics.Exif
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
  if (length args) == 1
       then processFile $ head args
       else putStrLn "give a filename as parameter"

debug :: IO()
debug = processFile "/home/roland/Temp/DSC04847.JPG"


processFile :: String -> IO()
processFile filename = do
   exif <- fromFile $ filename

   mbLatt <- getTag exif "InteroperabilityVersion"
   mbLong <- getTag exif "GPSLongitude"

   let eLatt = parse "Lattiude" mbLatt
   let eLong = parse "Longitude" mbLong

   showResult $ lattLong2Sk <$> eLatt <*> eLong


parse :: String -> Maybe String -> Either String Degree
parse c (Just s)  = getCoord c s
parse c Nothing = Left $ c ++ " No input"


getUrl :: CH1903 -> String
getUrl (CH long latt) = 
    printf "http://map.geo.admin.ch/?&Y=%d&X=%d&zoom=8&crosshair=bowl" long latt

showResult :: Either String CH1903 -> IO()
showResult (Left e) = putStrLn e
showResult (Right ch) = do
    openBrowserOn (getUrl ch)
    return ()





