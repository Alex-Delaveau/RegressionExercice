module Main where

--hasktorch

import Torch.Functional (squeezeAll)
--hasktorch-tools

import Torch.Control (foldLoop, mapAccumM)
import Torch.Device (Device (..), DeviceType (..))
import Torch.Functional (add, mseLoss)
import Torch.Layer.Linear (LinearHypParams (..), linearLayer)
import Torch.NN (sample)
import Torch.Optim (GD (..))
import Torch.Tensor (asValue)
import Torch.Tensor.TensorFactories (asTensor'',zeros')
import Torch.Train (loadParams, saveParams, showLoss, update, zeroTensor)
import Torch.Util.Chart (drawLearningCurve)

import Graphics.Gnuplot.Simple

--Data
import Text.CSV

toData :: [[String]] -> [([Float], Float)]
toData x = map (\[xs,x] -> ([xs],x)) $ map (map (read::String->Float)) $ tail x


dataTuple :: [[String]] -> [(Float, Float)]
dataTuple x =  map (\[xs,x] -> (xs,x)) $ map (map (read::String->Float)) $ tail x


main :: IO ()
main = do
  --CSV
  let fileName = "datasets/test.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  case csv of 
      Left e -> print ("Parse error" ++ show e)
      Right y -> print "Parse Ok"
  let Right dataString = csv
  let dataSet = toData dataString
  let trainingData = tail dataSet
  let testData = [head dataSet]
  let tuples = dataTuple dataString
  --Training
  let iter = 100 :: Int
      device = Device CPU 0
  initModel <- sample $ LinearHypParams device 1 1
  ((trainedModel, _), losses) <- mapAccumM [1 .. iter] (initModel, GD) $ \epoc (model, opt) -> do
    let batchLoss = foldLoop trainingData zeroTensor $ \(input, output) loss ->
          let y' = linearLayer model $ asTensor'' device input
              y = asTensor'' device output
           in add loss $ mseLoss y y'
        lossValue = (asValue batchLoss) :: Float
    showLoss 5 epoc lossValue
    u <- update model opt batchLoss 5e-8
    return (u, lossValue)
  saveParams trainedModel "regression.model"
  --mapM_ (putStr . printf "%2.3f ") $ reverse allLosses
  loadedModel <- loadParams (LinearHypParams device 1 1) "regression.model"
--IO
  let output = linearLayer loadedModel $ asTensor'' device $ fst $ head testData
      y' = (asValue output) :: Float
      y = snd $ head testData
      tensor = asTensor'' device [(1::Int)]
      bias = linearLayer loadedModel $ zeros' device [(1::Int)]
      weight = linearLayer loadedModel tensor
      biasValue = (asValue bias) :: Float
      weightValue = ((asValue weight) :: Float) - biasValue
  
  drawLearningCurve "plots/graph-reg.png" "Learning Curve" [("", reverse losses)]
  plotDots [(PNG "plots/list.png"),(Title "Training set")] tuples
  plotFunc [(PNG "plots/funcPlot.png"),(Title "List"),(XLabel "X"),(YLabel "Y")] (linearScale 10000 (0,100)) (\x-> (weightValue*x + biasValue))
  print loadedModel
  putStr "\nPrediction: "
  print y'
  putStr "Ground truth: "
  print y
  putStr "Mse: "
  print ((y' - y) * (y' - y))



