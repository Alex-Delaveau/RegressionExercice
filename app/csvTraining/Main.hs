module Main where

--CSV
import Text.CSV

main :: IO()
main = do
    let fileName = "datasets/test.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    case csv of 
        Left e -> print ("Parse error" ++ show e)
        Right y -> print "Parse Ok"
    let Right dataString = csv
    let dataFloat = toData dataString
    let tuples = dataTuple dataString
    print tuples


toData :: [[String]] -> [([Float], Float)]
toData x = map (\[xs,x] -> ([xs],x)) $ map (map (read::String->Float)) $ tail x

dataTuple :: [[String]] -> [(Float, Float)]
dataTuple x =  map (\[xs,x] -> (xs,x)) $ map (map (read::String->Float)) $ tail x
