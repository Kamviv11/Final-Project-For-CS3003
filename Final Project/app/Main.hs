module Main where
import System.IO ()
import System.Random

-- A function to get a random element from a list
-- It operates within the IO monad.
getRandomElement :: [a] -> IO a
getRandomElement xs = do
    -- Get the length of the list to determine the index range
    let len = length xs
    -- Generate a random index. Note: list indices are 0-based.
    index <- randomRIO (0, len - 1)
    -- Return the element at the random index
    return (xs !! index)

main :: IO ()
main = do
    let file = "words.txt"
    fileContent <- readFile file
    let wordList = lines fileContent
    randomWord <- getRandomElement wordList
    putStrLn randomWord