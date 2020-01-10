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

type Column a = [a]

column :: (Read a) => String -> CSV' -> Either String (Column a)
column name rows = do
  i <-
    maybe (Left ("Column " <> name <> " not found.")) return
      . findIndex (name ==)
      $ NE.head rows
  return . catMaybes . fmap ((=<<) readMaybe . (^? element i)) $ NE.tail rows

columnAverage :: Column Double -> Double
columnAverage = (/) <$> sum <*> genericLength

applyToColumn :: (Read a) => (Column a -> b) -> String -> CSV' -> Either String b
applyToColumn f name = (=<<) (return . f) . column name

applyToCSVColumn :: (Read a) => (Column a -> b) -> String -> FilePath -> IO (Either String b)
applyToCSVColumn f name path = do
  eRows <- parseCSVFromFile path
  return $
    applyToColumn f name
      =<< maybe (Left "CSV is empty.") return . NE.nonEmpty
      =<< either (Left . show) return eRows

main :: IO ()
main = either error print =<< applyToCSVColumn columnAverage "mag" "all_week.csv"
