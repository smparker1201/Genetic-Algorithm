module RandomStuff where

import Data.List
import Prelude hiding (flip)
import System.Random

type Organism = [Int]

flip :: [Int] -> Int -> [Int]
flip [] i = []
flip (0 : xs) 0 = 
    1 : xs
flip (1 : xs) 0 = 
    0 : xs
flip (x : xs) i = 
    x : (flip xs (i - 1))

mutate :: Organism -> IO Organism
mutate xs = do
    i <- randomIO :: IO Int
    return $ flip xs (i `mod` length xs)

fitness [] = 0
fitness (x:xs) | (x == 1) = 1 + fitness xs
               | otherwise = fitness xs

breed :: Organism -> Organism -> [Organism]
breed xs ys = [(take (len `div` 2) xs) ++ (take (len `div` 2) ys)
              ,(take (len `div` 2) xs) ++ (drop (len `div` 2) ys)
              ,(drop (len `div` 2) xs) ++ (take (len `div` 2) ys)
              ,(drop (len `div` 2) xs) ++ (drop (len `div` 2) ys)]
    where len = length xs

randomInts :: IO [Int]
randomInts = do
    gen <- newStdGen
    return $ randomRs (0, 1) gen

randomOrganism :: IO Organism
randomOrganism = fmap (take 32) randomInts

sample :: Int -> [a] -> [[a]] 
sample 0 _ = [[]] 
sample _ [] = [] 
sample k (x:xs) = 
    let withFirst = map (x:) $ sample (k - 1) xs 
        withoutFirst = sample k xs 
    in withFirst ++ withoutFirst

orgy :: [Organism] -> [Organism]
orgy potentialParents = [ child | [mom, dad] <- sample 2 potentialParents,
                                  child <- breed mom dad ]

simulate :: [Organism] -> IO [Organism]
simulate parents = do
    mutatedChildren <- mapM mutate (orgy parents)
    let bestChildren = sortBy (\childA childB -> compare (fitness childA) (fitness childB)) mutatedChildren
    return $ take (length bestChildren `div` 2) bestChildren

runSimulation :: Int -> [Organism] -> IO [Organism]
runSimulation 0 generation = return generation
runSimulation n generation = do
    putStrLn "****** GENERATION ********"
    mapM_ (putStrLn . show . fitness) generation
    nextGeneration <- simulate generation
    runSimulation (n - 1) nextGeneration

main = do
    runSimulation 50 [replicate 32 1, replicate 32 1]
