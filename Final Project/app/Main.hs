module Main where
import System.IO (readFile)
import System.Random (getStdGen, randomR)

main :: IO ()
main = do
    let file = "words.txt"
    fileContent <- readFile file
    let wordList = lines fileContent
