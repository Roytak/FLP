-----------------------------------------------------------------------
--- FLP Project 1 - Decision Tree Classifier
--- Author: Roman Janota
--- Email: xjanot04@fit.vutbr.cz
--- Date: 29.03.2025
-----------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List(maximumBy, nub, sort)

usage :: String
usage = "Usage: flp-fun [-1 <treeFile> <dataFile> | -2 <trainingDataFile>]\n" ++
        "  -1 <treeFile> <dataFile> : Classify data using the decision tree from <treeFile> on <dataFile>\n" ++
        "  -2 <trainingDataFile>    : Build a decision tree from the training data in <trainingDataFile>\n"

data DecisionTree = Node
    {
        feature :: Int,         -- index of a feature
        threshold :: Float,     -- threshold value
        left :: DecisionTree,   -- left subtree
        right :: DecisionTree   -- right subtree
    }
    | Leaf
    {
        classLabel :: String    -- class label
    }
    deriving (Eq)

-- custom show for DecisionTree, to print it with indentation
instance Show DecisionTree where
    show tree = showTree tree 0
        where
            showTree :: DecisionTree -> Int -> String
            showTree (Leaf label) indent = repeatSpaces indent ++ "Leaf: " ++ label
            showTree (Node feature threshold left right) indent =
                repeatSpaces indent ++ "Node: " ++ show feature ++ ", " ++ show threshold ++ "\n" ++
                showTree left (indent + 1) ++ "\n" ++
                showTree right (indent + 1)
            repeatSpaces :: Int -> String
            repeatSpaces 0 = ""
            repeatSpaces n = "  " ++ repeatSpaces (n-1)

-- parse tree file
parseTreeF :: FilePath -> IO DecisionTree
parseTreeF filePath = do
    input <- readFile filePath
    let parsed = map (map (filter (/= ',')) . words) (lines input)  -- split on commas and remove them
    return $ parseTree parsed

-- parse the tree from the input file
parseTree :: [[String]] -> DecisionTree
parseTree input = fst $ parseTreeRec input
    where
        parseTreeRec :: [[String]] -> (DecisionTree, [[String]])        -- recursively parse the tree
        parseTreeRec [] = error "Parsing tree failed: Tree is empty."
        parseTreeRec (x:xs)
            | firstElem == "Node:" =
                let feature = read (x !! 1) :: Int
                    threshold = read (x !! 2) :: Float
                    (leftTree, remainingLinesAfterLeft) = parseTreeRec xs
                    (rightTree, remainingLinesAfterRight) = parseTreeRec remainingLinesAfterLeft
                in (Node feature threshold leftTree rightTree, remainingLinesAfterRight)
            | firstElem == "Leaf:" =
                let classLabelVal = x !! 1
                in (Leaf classLabelVal, xs)
            | otherwise = error "Parsing tree failed: Invalid structure."
            where
                firstElem = case x of   -- get first element of the line
                    [] -> error "Parsing tree failed: Empty line."
                    (y:_) -> y

-- parse data file
parseDataF :: FilePath -> IO [[Float]]
parseDataF filePath = do
    input <- readFile filePath
    let values = map (map read . words . map (\c -> if c == ',' then ' ' else c)) (lines input)    -- split on commas and convert to floats
    return values

-- classify data using a given decision tree
classify :: DecisionTree -> [[Float]] -> [String]
classify tree data_ = map (classifyData tree) data_
    where
        classifyData :: DecisionTree -> [Float] -> String
        classifyData (Leaf classLabel) _ = classLabel
        classifyData (Node feature threshold left right) dataRow
            | dataRow !! feature < threshold = classifyData left dataRow
            | otherwise = classifyData right dataRow

-- parse training data file
parseTrainingDataF :: FilePath -> IO [([Float], String)]
parseTrainingDataF trainingDataFile = do
    input <- readFile trainingDataFile
    let values = map (parseTrainingDataLine . words . map replaceComma) (lines input)
    return values
    where
        replaceComma :: Char -> Char
        replaceComma ',' = ' '
        replaceComma c   = c

        parseTrainingDataLine :: [String] -> ([Float], String)
        parseTrainingDataLine line = (map read (init line), last line)

-- gini index calculation
gini :: [([Float], String)] -> Float
gini trainingData = 1 - sum [p * p | p <- probs]        -- 1 - sum of squares of probabilities
    where
        total = fromIntegral $ length trainingData
        probs = [fromIntegral (length $ filter (\(_, label) -> label == c) trainingData) / total | c <- classes]
        classes = nub $ map snd trainingData

-- split data into left and right based on feature index and threshold value
splitData :: Int -> Float -> [([Float], String)] -> ([([Float], String)], [([Float], String)])
splitData featureIndex thresholdValue trainingData = (leftData, rightData)
    where
        leftData = filter (\(features, _) -> features !! featureIndex < thresholdValue) trainingData
        rightData = filter (\(features, _) -> features !! featureIndex >= thresholdValue) trainingData

-- calculate Gini gain for a given feature index and threshold value
giniGain :: [([Float], String)] -> Int -> Float -> Float
giniGain parentData featureIndex thresholdValue = parentGini - (leftWeight * leftGini + rightWeight * rightGini)
    where
        (leftData, rightData) = splitData featureIndex thresholdValue parentData        -- split data into left and right on the given feature and threshold
        parentGini = gini parentData
        leftGini = gini leftData
        rightGini = gini rightData
        leftWeight = fromIntegral (length leftData) / fromIntegral (length parentData)
        rightWeight = fromIntegral (length rightData) / fromIntegral (length parentData)

-- check if the split is invalid (all data points belong to the same class or if there are no data points)
isSplitInvalid :: [([Float], String)] -> Bool
isSplitInvalid trainingData = length (nub $ map snd trainingData) == 1 || length trainingData == 0

-- custom head function to avoid a warning due to a use of partial function 'head' in findBestSplit even though it is safe
myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

-- find the best split for the training data
findBestSplit :: [([Float], String)] -> (Int, Float, Float)
findBestSplit trainingData =
    if isSplitInvalid trainingData
    then
        (-1, 0.0, -1.0)    -- invalid split indicator
    else
      let numFeatures = length $ fst $ myHead trainingData
          allSplits =
            [ (featureIndex, thresholdValue, giniGain trainingData featureIndex thresholdValue)
              | featureIndex <- [0 .. numFeatures - 1],
                thresholdValue <- nub $ sort $ map (\(features, _) -> features !! featureIndex) trainingData
            ]   -- generate all possible splits
       in maximumBy (\(_, _, gain1) (_, _, gain2) -> compare gain1 gain2) allSplits     -- find the split with the maximum gini gain

-- build the decision tree recursively from the training data
buildTree :: [([Float], String)] -> DecisionTree
buildTree trainingData =
    let (featureIndex, thresholdValue, _) = findBestSplit trainingData
    in  if featureIndex == -1
        then
            -- an invalid split, return a leaf node with the most common class label
            let
                classCounts = [(label, length $ filter (\(_, l) -> l == label) trainingData) | label <- nub $ map snd trainingData]
                mostCommonLabel = fst $ maximumBy (\(_, count1) (_, count2) -> compare count1 count2) classCounts
            in Leaf {classLabel = mostCommonLabel}
        else
            -- a valid split, create a node and recursively build the left and right subtrees
            let (leftData, rightData) = splitData featureIndex thresholdValue trainingData
            in Node
                {
                    feature = featureIndex,
                    threshold = thresholdValue,
                    left = buildTree leftData,
                    right = buildTree rightData
                }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        ["-1", treeFile, dataFile] -> do
            tree <- parseTreeF treeFile                         -- parse the tree file
            tree_data <- parseDataF dataFile                    -- parse the data file
            putStrLn $ unlines $ classify tree tree_data        -- classify data
        ["-2", trainingDataFile] -> do
            trainingData <- parseTrainingDataF trainingDataFile -- parse the training data file
            let trainedTree = buildTree trainingData            -- build the decision tree
            print trainedTree                                   -- print the decision tree
        _ -> putStrLn usage
