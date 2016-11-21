module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Either
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Data.Monoid
import Control.Exception.Base (assert)
import Control.Monad (when)

import Facebook.Gen.Csv
import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStr (genFiles)

csvFiles = V.fromList ["data/adaccount.csv", "data/adcampaign.csv", "data/adset.csv",
                       "data/adimage.csv", "data/adcreative.csv", "data/ad.csv", "data/adlabel.csv",
                       "data/customaudience.csv", "data/adspixel.csv"]

main :: IO ()
main = do
  inps <- V.mapM BS.readFile csvFiles
  let csvs = V.map (decode HasHeader) inps :: V.Vector (Either String (V.Vector CsvLine))
  let l = V.toList csvs
  when (V.length csvs /= Prelude.length l) $ error $ "parsed output is not the same length as unparsed input"
  when (Prelude.length (lefts l) /= 0) $ error $ "Some CSVs did not parse: " ++ show (lefts l)
  putStrLn "Generating source code ..."
  let env = buildEnv $ V.fromList $ rights l
  V.mapM_ saveFile $ genFiles env

saveFile :: (FilePath, Text) -> IO ()
saveFile (path, data_) = do
  putStrLn $ "Saving " ++ path
  T.writeFile ("../src/" <> path) data_
