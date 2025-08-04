module Main where
import System.IO ()
import System.Random
import Distribution.SPDX (LicenseId(NullBSD))
import Data.List
import Text.Parsec (State(State))

data State = Correct | Misplaced | Incorrect

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

isLetterCorrect :: String -> String -> Int -> String
isLetterCorrect answer (x:xs) indexOfChar = do
    let allIndices = elemIndices x answer
    if null allIndices 
        then x + "Is Incorrect" + isLetterCorrect answer xs (indexOfChar + 1) 
        else helper2 indexOfChar allIndices + isLetterCorrect answer xs (indexOfChar + 1)
    where
        helper2 :: Int -> [Int] -> String
        helper2 _ [] = x + "is Misplaced"
        helper2 x (y:ys) = if x == y then x + "is Correct" else helper2 x ys

GameLogic :: Int -> String -> Nothing
GameLogic x answer = do
    putStr "Enter your guess (5 letters)"
    hFlush stdout --input buffer
    guess <- getLine
    let isLetterCorrect answer guess 0
    putStr "Guess " + x + "/6"



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

    let current_guesses = 0
    let MAX_guesses = 6
    --Getting player input
    putStr "Enter your guess (5 letters)"
    hFlush stdout --input buffer
    guess <- getLine

    