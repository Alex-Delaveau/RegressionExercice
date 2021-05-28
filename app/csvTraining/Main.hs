module Main where

--CSV
import Text.CSV

main :: IO()
main = do
    let fileName = "test.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv
handleError csv = putStrLn "error parsing"
doWork csv = print $ toData csv

toData :: [[String]] -> [([Float], Float)]
toData x = map (\[xs,x] -> ([xs],x)) $ map (map (read::String->Float)) $ tail x