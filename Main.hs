type Inputs = [Float]
type Weights = [Float]
type Output = Float

-- NEURON 
data Neuron = Neuron { weights  :: Weights 
                     , bias     :: Float 
                     }

instance Show Neuron where 
    show n = "Weights: " ++ show (weights n) ++ " Bias: " ++ show (bias n)

adjust :: Neuron -> [Float] -> Float -> Neuron 
adjust old wamount bamount = Neuron { weights = zipWith (+) (weights old) (wamount ++ repeat 0) 
                                    , bias    = (bias old) + bamount
                                    }

adjustNthWeight :: Neuron -> Int -> Float -> Neuron 
adjustNthWeight neuron pos ammount = adjust neuron (replicate pos 0 ++ [ammount]) 0

compute :: Inputs -> Neuron -> Float
compute i n = tanh $ (bias n) + (sum $ zipWith (*) (weights n) i)

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
train neuron tdata = adjust neuron byWeights byBias
    where 
        ccost       = cost neuron tdata
        cbias       = bias neuron
        cweights    = weights neuron
        eps         = 1e-3
        rate        = 1e-4
        nInCo       = length (weights neuron)
        adjWCosts n = if n == nInCo then [] 
                      else cost (adjustNthWeight neuron n eps) tdata : adjWCosts (n+1)
        dws         = map (/eps) $ zipWith (-) (repeat ccost) (adjWCosts 0)
        byWeights   = (map (*rate) dws)
        adjBCost    = cost (Neuron cweights (cbias + eps)) tdata
        bdw         = (ccost - adjBCost) / eps
        byBias      = bdw * rate

-- Pseudo or-gate
testNeuron = Neuron{ weights = [1, 1], bias = 0.0 }
trainingData = 
    [ TrainingDatum { inputs = [0, 0], expected = 0 }
    , TrainingDatum { inputs = [1, 0], expected = 1 }
    , TrainingDatum { inputs = [0, 1], expected = 1 }
    , TrainingDatum { inputs = [1, 1], expected = 1 }
    ]

putNeuronComputing :: Neuron -> TrainingData -> IO ()
putNeuronComputing _ []     = return ()
putNeuronComputing n (d:ds) = do 
    putStrLn $ sinput d ++ sexpected d ++ scomputed d
    putNeuronComputing n ds
    where 
        sinput    x = "Input: " ++ show (inputs x)
        sexpected x = ", Expected: " ++ show (expected x)
        scomputed x = ", Computed: " ++ show (compute (inputs x) n)

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
    putNeuronComputing result tdata
    putStrLn " -- -- -- -- -- " 
    putStr "Repeat?[yes]:"
    ans <- getLine 
    if ans == "y" || ans == "yes" || ans == "" then doTraining result tdata 
    else return ()
    where 
        doIter 0 neuron = neuron
        doIter i neuron = doIter (i-1) (train neuron tdata)

main = doTraining testNeuron trainingData
