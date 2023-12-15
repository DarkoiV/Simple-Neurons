type Inputs = [Float]
type Weights = [Float]
type Output = Float

data Neuron = Neuron { weights :: Weights }

instance Show Neuron where 
    show n = "Weights: " ++ show (weights n)

adjustWeights :: Neuron -> Float -> Neuron 
adjustWeights old by = Neuron { weights = map (+ by) (weights old) }

data TrainingDatum = TrainingDatum { inputs   :: Inputs
                                   , expected :: Float 
                                   }
type TrainingData = [TrainingDatum]
len ::TrainingData -> Float 
len = fromIntegral . length

computeNeuron :: Neuron -> Inputs -> Float
computeNeuron n i = foldr (+) 0 calcWeights 
    where 
        calcWeights = zipWith (*) (weights n) i

cost :: Neuron -> TrainingData -> Float
cost neuron tdata = diffsum^2 / len tdata 
    where 
        diffsum         = foldl (+) 0.0 (map diff tdata)
        diff datum      = (computed datum) - (expected datum)
        computed datum  = computeNeuron neuron (inputs datum)

train :: Neuron -> TrainingData -> Int -> Neuron
train neuron _ 0         = neuron
train neuron tdata times = train result tdata (times - 1)
    where 
        eps     = 1e-3
        ccost   = cost neuron tdata
        ecost   = cost (adjustWeights neuron eps) tdata
        dw      = (ccost - ecost) / eps
        rate    = 1e-3
        result  = adjustWeights neuron (rate * dw) 

-- Pseudo neuron doubling value
testNeuron = Neuron{weights = [0.5]}
trainingData = 
    [ TrainingDatum { inputs = [0], expected = 0 }
    , TrainingDatum { inputs = [1], expected = 2 }
    , TrainingDatum { inputs = [2], expected = 4 }
    , TrainingDatum { inputs = [3], expected = 6 }
    , TrainingDatum { inputs = [4], expected = 8 }
    ]

main :: IO ()
main = do 
    putStrLn "Hello Neuron!"
    let c = cost testNeuron trainingData 
    putStrLn $ "Neuron before training: " ++ show testNeuron
    putStrLn $ "Cost before training: " ++ show c
    let trainedNeuron = train testNeuron trainingData 1000
    let c = cost trainedNeuron trainingData 
    putStrLn $ "Cost after training: " ++ show c
    putStrLn $ "Neuron after training: " ++ show trainedNeuron
