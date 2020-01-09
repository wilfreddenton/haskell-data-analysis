{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.List
import Data.Maybe (catMaybes)
import Text.CSV
import Text.Read (readMaybe)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

columnIndex :: String -> CSV -> Either String Int
columnIndex name = maybe err return . (=<<) (findIndex (name ==)) . mHeaders
  where
    err = Left ("Column " <> name <> " not found.")
    mHeaders [] = Nothing
    mHeaders (x : _) = Just x

column :: Int -> CSV -> Either String [String]
column i = return . catMaybes . fmap (^? element i) . tail

columnAverage :: [String] -> Double
columnAverage = average . catMaybes . fmap (readMaybe @Double)

applyToColumn :: ([String] -> b) -> String -> CSV -> Either String b
applyToColumn f name rows = return . f =<< flip column rows =<< columnIndex name rows

applyToCSVColumn :: ([String] -> b) -> String -> FilePath -> IO (Either String b)
applyToCSVColumn f name =
  fmap ((=<<) (applyToColumn f name) . either (Left . show) return) . parseCSVFromFile

main :: IO ()
main = either error print =<< applyToCSVColumn columnAverage "mag" "all_week.csv"
