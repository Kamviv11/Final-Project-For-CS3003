module Main where
import System.IO (readFile)

main :: IO ()
main = do
    let words = "words.txt"
    wordscontent <- readFile words
    putStrLn wordscontent
