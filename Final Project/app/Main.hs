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
    --Takes in the file contents of "words.txt" and puts it into fileContent
    let file = "words.txt"
    fileContent <- readFile file
    --Makes a list and has each element as a line of the txt file
    let wordList = lines fileContent
    --gets a random word and puts it into randomWord
    randomWord <- getRandomElement wordList
    putStrLn randomWord