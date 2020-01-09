{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Text.CSV
import Text.Read (readMaybe)

type CSV' = NonEmpty Record

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

columnIndex :: String -> CSV' -> Either String Int
columnIndex name =
  maybe (Left ("Column " <> name <> " not found.")) return
    . findIndex (name ==)
    . NE.head

column :: Int -> CSV' -> Either String [String]
column i = return . catMaybes . fmap (^? element i) . NE.tail

columnAverage :: [String] -> Double
columnAverage = average . catMaybes . fmap (readMaybe @Double)

applyToColumn :: ([String] -> b) -> String -> CSV' -> Either String b
applyToColumn f name rows = return . f =<< flip column rows =<< columnIndex name rows

applyToCSVColumn :: ([String] -> b) -> String -> FilePath -> IO (Either String b)
applyToCSVColumn f name path = do
  eRows <- parseCSVFromFile path
  return $
    applyToColumn f name
      =<< maybe (Left "CSV is empty.") return . NE.nonEmpty
      =<< either (Left . show) return eRows

main :: IO ()
main = either error print =<< applyToCSVColumn columnAverage "mag" "all_week.csv"
