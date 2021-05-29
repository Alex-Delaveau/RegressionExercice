module Main where

import Graphics.Gnuplot.Simple

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
    plotDots [(PNG "listPlot.png"),(Title "List"),(XLabel "X"),(YLabel "Y")] tuples
    plotFunc [(PNG "funcPlot.png"),(Title "List"),(XLabel "X"),(YLabel "Y")] (linearScale 10000 (0,100)) function



toData :: [[String]] -> [([Float], Float)]
toData x = map (\[xs,x] -> ([xs],x)) $ map (map (read::String->Float)) $ tail x

dataTuple :: [[String]] -> [(Float, Float)]
dataTuple x =  map (\[xs,x] -> (xs,x)) $ map (map (read::String->Float)) $ tail x

function :: Float -> Float
function x =  1.0296 * x - 1.4968