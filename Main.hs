type Inputs = [Float]
type Weights = [Float]
type Output = Float

-- NEURON 
data Neuron = Neuron { weights :: Weights }

instance Show Neuron where 
    show n = "Weights: " ++ show (weights n)

adjust :: Neuron -> [Float] -> Neuron 
adjust old amount = Neuron { weights = zipWith (+) (weights old) (amount ++ repeat 0) }

compute :: Inputs -> Neuron -> Float
compute i n = sum $ zipWith (*) (weights n) i 

-- TRAINING DATA
data TrainingDatum = TrainingDatum { inputs   :: Inputs
                                   , expected :: Float 
                                   }
type TrainingData = [TrainingDatum]

cost :: Neuron -> TrainingData -> Float
cost neuron tdata = diffsqr / len
    where 
        indCost datum = compute (inputs datum) neuron - (expected datum)
        diffsqr       = (^2) . sum $ map indCost tdata
        len           = fromIntegral $ length tdata

train :: Neuron -> TrainingData -> Neuron 
train neuron tdata = adjust neuron (map (*rate) dws)
    where 
        ccost     = cost neuron tdata
        eps       = 1e-3
        rate      = 1e-3
        nInCo     = length (weights neuron)
        adjCost n = if n == nInCo then [] 
                    else cost (adjust neuron (replicate n 0 ++ [eps])) tdata : adjCost (n+1)
        dws       = map (/eps) $ zipWith (-) (repeat ccost) (adjCost 0)

-- Pseudo neuron doubling value
testNeuron = Neuron{weights = [0.5]}
trainingData = 
    [ TrainingDatum { inputs = [0], expected = 0 }
    , TrainingDatum { inputs = [1], expected = 2 }
    , TrainingDatum { inputs = [2], expected = 4 }
    , TrainingDatum { inputs = [3], expected = 6 }
    , TrainingDatum { inputs = [4], expected = 8 }
    ]

doTraining :: Neuron -> TrainingData -> IO ()
doTraining neuron tdata = do
    putStrLn $ show neuron 
    putStrLn $ "Cost: " ++ show (cost neuron tdata)
    putStrLn "How many iterations?"
    iter <- getLine
    let result = doIter (read iter) neuron
    putStrLn "      RESULT    "
    putStrLn " -- -- -- -- -- " 
    putStrLn $ show result
    putStrLn $ "Cost: " ++ show (cost result tdata)
    putStrLn " -- -- -- -- -- " 
    putStr "Repeat?[yes]:"
    ans <- getLine 
    if ans == "y" || ans == "yes" || ans == "" then doTraining result tdata 
    else return ()
    where 
        doIter 0 neuron = neuron
        doIter i neuron = doIter (i-1) (train neuron tdata)

main = doTraining testNeuron trainingData
