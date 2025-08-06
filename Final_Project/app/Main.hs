module Main where

import System.IO
import System.Random
import Data.List
--  The additional imports are for the second part of the project.
--  The second part of project is to implement a letter tracker
--  The tracker will keep track of the letters that are in the word
--  and their positions, and will display the results in a more user-friendly way in each round. 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toUpper)


data State = Incorrect | Misplaced | Correct deriving (Eq, Show, Ord)

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

isGuessCorrect :: String -> String -> [State]
isGuessCorrect answer guess = zipWith helper2 answer guess
    where
        helper2 :: Char -> Char -> State
        helper2 answerChar guessChar
            | answerChar == guessChar = Correct
            | guessChar `elem` answer   = Misplaced
            | otherwise               = Incorrect

formatLetter :: Char -> State -> String
formatLetter c Correct   = "🟩" ++ [c] ++ "🟩"
formatLetter c Misplaced = "🟨" ++ [c] ++ "🟨"
formatLetter c Incorrect = "🟥" ++ [c] ++ "🟥"

displayResult :: String -> [State] -> IO ()
displayResult guess states = do
    let formattedResult = zipWith formatLetter guess states
    putStrLn ("Result:  " ++ unwords formattedResult ++ "\n")


-- The new function to display the keyboard state.
displayKeyboardState :: Map Char State -> IO ()
displayKeyboardState keyboardState = do
    putStrLn "Keyboard State:"
    let rows = ["QWERTYUIOP", "ASDFGHJKL","ZXCVBNM"]
    mapM_ (putStrLn . formatRow) rows
    putStrLn "----------------------\n"
    where
        formatRow :: String -> String
        formatRow = unwords . map formatKey
        formatKey :: Char -> String
        formatKey char =
          -- Find the state of the key, defaulting to Incorrect (or Unguessed)
          let state = Map.findWithDefault Incorrect char keyboardState
          in formatLetter char state

-- The additional function to elevate this game to a more user-friendly level.
-- The keyboard state will be updated after each guess.
updateKeyboardState :: String -> [State] -> Map Char State -> Map Char State   
updateKeyboardState guess states oldKeyboardState =
    let 
        charStates = zip (map toUpper guess) states
        update_map kbd (char, state) = Map.insertWith max char state kbd
    in 
        foldl' update_map oldKeyboardState charStates

-- Implemented the new keyboard state in the game logic.

gameLogic :: String -> Int -> Map Char State -> IO()
gameLogic answer guess_NUM keyboardState = 
    if guess_NUM == 6 then 
        putStrLn ("You lost the game! The word was " ++ answer)
    else do
        --Added the keyboard state to the game logic to be displayed
        displayKeyboardState keyboardState
        putStrLn "Enter your guess (5 letters)"
        hFlush stdout --input buffer
        guess <- getLine

        if length guess /= length answer then do
            putStrLn "Invalid guess length. Please try again."
            gameLogic answer guess_NUM keyboardState
        else if guess == answer then do
            displayResult guess (isGuessCorrect answer guess)
            putStrLn "Congratulations, you won!"
        else do
            displayResult guess (isGuessCorrect answer guess)
            putStrLn ("This is Guess " ++ show(guess_NUM + 1) ++ "/6")
            putStrLn ""
            gameLogic answer (guess_NUM + 1) keyboardState
            --where
                -- Update the keyboard state with the current guess and its result
              --  newKeyboardState = updateKeyboardState guess (isGuessCorrect answer guess) keyboardState

main :: IO ()
main = do
    putStrLn "--- Welcome to Haskell Wordle ---"
    --Takes in the file contents of "words.txt" and puts it into fileContent
    let file = "words.txt"
    fileContent <- readFile file
    --Makes a list and has each element as a line of the txt file
    let wordList = lines fileContent
    --gets a random word and puts it into randomWord
    randomWord <- getRandomElement wordList
    gameLogic randomWord 0 Map.empty 
-- The initial keyboard state is empty, meaning no letters have been guessed yet.
-- The keyboard state will be updated as the game progresses.

    