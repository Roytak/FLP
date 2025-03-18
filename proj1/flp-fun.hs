import System.Environment (getArgs)
import System.IO

usage :: String
usage = "Usage: flp-fun [-1 <treeFile> <dataFile> | -2 <treeFile>]"

data DecisionTree = Node
    {
        feature :: Int,
        threshold :: Float,
        left :: DecisionTree,
        right :: DecisionTree
    }
    | Leaf
    {
        classLabel :: String
    }
    deriving (Show)

-- Parse a decision tree from a file
parseTree :: String -> IO DecisionTree
parseTree file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    let linesOfFile = lines contents
    let tree = parseTree' linesOfFile
    hClose handle
    return tree

parseTree' :: [String] -> DecisionTree
parseTree' [] = error "Empty tree file."
parseTree' (line:rest) = 
    case words line of
        ["Node :", f, t] 

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        ["-1", treeFile, dataFile] -> do
            putStrLn "Option 1 selected."
            putStrLn $ "Tree file: " ++ treeFile
            putStrLn $ "Data file: " ++ dataFile
            tree <- parseTree treeFile
            putStrLn $ show tree
        ["-2", treeFile] -> do
            putStrLn "Option 2 selected."
            putStrLn $ "Tree file: " ++ treeFile
        _ -> putStrLn usage
        