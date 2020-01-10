{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad ((<=<))
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
  i <- maybe err return . findIndex (name ==) $ NE.head rows
  return . catMaybes . fmap (readMaybe <=< (^? element i)) $ NE.tail rows
  where
    err = Left ("Column " <> name <> " not found.")

columnAverage :: Column Double -> Double
columnAverage = (/) <$> sum <*> genericLength

applyToColumn :: (Read a) => (Column a -> b) -> String -> CSV' -> Either String b
applyToColumn f name = fmap f . column name

applyToCSVColumn :: (Read a) => (Column a -> b) -> String -> CSV -> Either String b
applyToCSVColumn f name = applyToColumn f name <=< maybe (Left "CSV is empty.") return . NE.nonEmpty

main :: IO ()
main = do
  eRows <- either (Left . show) return <$> parseCSVFromFile "all_week.csv"
  either error print (applyToCSVColumn columnAverage "mag" =<< eRows)
