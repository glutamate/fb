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

import Facebook.Gen.Csv
import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStr

csvFiles = V.fromList ["data/adaccount.csv", "data/adcampaign.csv", "data/adset.csv",
                       "data/adimage.csv", "data/adcreative.csv", "data/ad.csv", "data/adlabel.csv"]

main :: IO ()
main = do
    inps <- V.mapM BS.readFile csvFiles
    let csvs = V.map (decode HasHeader) inps :: V.Vector (Either String (V.Vector CsvLine))
    let l = V.toList csvs
    assert (V.length csvs == Prelude.length l) $ return ()
    putStrLn "Generating source code ..."
    let env = buildEnv $ V.fromList $ rights l
    saveFiles $ genFiles env

saveFiles :: V.Vector (FilePath, Text) -> IO ()
saveFiles = V.mapM_ save
    where
        save (path, data_) = do
            putStrLn $ "Saving " ++ path
            T.writeFile ("../src/" <> path) data_
