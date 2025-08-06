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

-- Define the possible states for each letter in the guess
-- Unguessed: The letter has not been guessed yet.
data State = Unguessed | Incorrect | Misplaced | Correct deriving (Eq, Show, Ord)

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

-- Updated function to correct the guess function and do two passes.
--The first pass checks if the guess is correct, misplaced, or incorrect.
-- The second pass updates the keyboard state based on the guess.
isGuessCorrect :: String -> String -> [State]
isGuessCorrect answer guess = 
    let 
        -- Convert both strings to uppercase for case-insensitive comparison
        answerUpper = map toUpper answer
        guessUpper = map toUpper guess
        -- First pass: determine the states of each letter 
        (pass1_results, remaining_pool) = findCorrectPass answerUpper guessUpper
        -- Second pass: determine misplaced and incorrect letters
        -- Use the remaining pool of letters from the answer
        final_states = findMisplacedPass pass1_results remaining_pool
    in 
        final_states
-- First pass checks for correct letters and builds a pool of remaining letters
findCorrectPass :: String -> String -> ([(Char, Bool)], String)
findCorrectPass answer guess = go answer guess [] []
    where
        --go :: String -> String -> String -> [(Char, Bool)] -> ([(Char, Bool)], String)
        go [] [] pool acc = (reverse acc, pool) -- Base case: we're done, return results.
        go (a:as) (g:gs) pool acc
      -- If letters match, it's a 'Correct' guess. Add (g, True) to our results.
      -- The answer letter 'a' is "used up" and NOT added to the pool.
            | a == g    = go as gs pool ((g, True) : acc)
      -- If letters don't match, it's not 'Correct'. Add (g, False) to our results.
      -- The answer letter 'a' is NOT used up, so we ADD it to the pool for the next pass.
            | otherwise = go as gs (a : pool) ((g, False) : acc)
        go _ _ _ _ = error "Input strings must have the same length" -- Edge case

-- The second pass checks for misplaced letters
-- It finds letters that are in the answer but not in the correct position.
        
findMisplacedPass :: [(Char, Bool)] -> String -> [State]
findMisplacedPass pass1_results pool = go pass1_results pool []
    where
    -- Arguments are: results from pass 1, the available letter pool, final states being built.
    --go :: [(Char, Bool)] -> String -> [State] -> [State]
    go [] _ acc = reverse acc -- Base case: we're done, return the final states.
    go ((g, isCorrect):gs) current_pool acc
      -- If the letter was already marked as Correct in pass 1, we're done with it.
      | isCorrect = go gs current_pool (Correct : acc)
      -- If it wasn't a correct match, check if the letter is in our pool.
      | otherwise =
          -- If the guess letter 'g' is in the pool of available letters...
          if g `elem` current_pool
          -- ...then it's a 'Misplaced' match. Add it to our results.
          -- CRUCIALLY, remove it from the pool so it can't be matched again.
          then go gs (delete g current_pool) (Misplaced : acc)
          -- If it's not in the pool, it's an 'Incorrect' match.
          else go gs current_pool (Incorrect : acc)



formatLetter :: Char -> State -> String
formatLetter c Unguessed = "🟦" ++ [c] ++ "🟦" -- New color for unused letters 
formatLetter c Correct   = "🟩" ++ [c] ++ "🟩"
formatLetter c Misplaced = "🟨" ++ [c] ++ "🟨"
formatLetter c Incorrect = "🟥" ++ [c] ++ "🟥"

-- Display the grid of guesses with their states
-- Each row will show the guess and its corresponding state.
--Takes over the displayResult function and displays the grid of guesses.
displayGrid :: [[(Char, State)]] -> IO ()
displayGrid guessHistory = do
    putStrLn "--- Your Guesses ---"
    let printRow row = putStrLn $ "  " ++ unwords (map (\(c, s) -> formatLetter (toUpper c) s) row)
    mapM_ printRow guessHistory
    let emptyRows = 6 - length guessHistory
    let emptyRow = "  " ++ unwords (replicate 5 $ formatLetter ' ' Unguessed)
    mapM_ (\_ -> putStrLn emptyRow) [1..emptyRows]
    putStrLn ""

-- The displayKeyboardState function will show the current state of the keyboard.
-- The new function to display the keyboard state.
displayKeyboardState :: Map Char State -> IO ()
displayKeyboardState keyboardState = do
    putStrLn "-----Keyboard State-----"
    let rows = ["QWERTYUIOP", "ASDFGHJKL","ZXCVBNM"]
    mapM_ (putStrLn . formatRow) rows
    putStrLn "----------------------\n"
    where
        -- Format each row of the keyboard
        formatRow :: String -> String
        formatRow = unwords . map formatKey
        -- Format each key in the row
        formatKey :: Char -> String
        formatKey char =
          -- Find the state of the key, defaulting to Unguessed
          let state = Map.findWithDefault Unguessed (toUpper char) keyboardState
          in formatLetter (toUpper char) state

-- The additional function to elevate this game to a more user-friendly level.
-- The keyboard state will be updated after each guess.
updateKeyboardState :: String -> [State] -> Map Char State -> Map Char State   
updateKeyboardState guess states oldKeyboardState =
    let 
        charStates = zip (map toUpper guess) states
        update_map keyboard (char, state) = Map.insertWith max char state keyboard
    in 
        foldl' update_map oldKeyboardState charStates

-- Implemented the new keyboard state in the game logic.
-- The game logic function will now take the keyboard state as an argument.
-- It will also display the keyboard state after each guess.
gameLogic :: String -> [[(Char, State)]] -> Map Char State -> IO()
gameLogic answer guessHistory keyboardState = 
    if length guessHistory == 6 then 
        putStrLn ("You lost the game! The word was " ++ answer)
    else do
        --Added the keyboard state to the game logic to be displayed
        displayGrid guessHistory
        displayKeyboardState keyboardState
        putStrLn "Enter your guess (5 letters)"
        hFlush stdout --input buffer
        guess <- getLine

        if length guess /= 5 then do
            putStrLn "Invalid guess length. Please try again."
            gameLogic answer guessHistory keyboardState
        else do
            -- Step 1: Calculate the new state efficiently, only once.
            -- Use the isGuessCorrect function to get the result states.
            -- This will return a list of states for each letter in the guess.
            let resultStates = isGuessCorrect answer guess
                newKeyboardState = updateKeyboardState guess resultStates keyboardState
                newGuessWithState = zip guess resultStates
                newHistory = guessHistory ++ [newGuessWithState]

            -- Provide immediate feedback by displaying the updated grid.
            displayGrid newHistory

            --  Use the familiar nested if-then-else structure.
            if map toUpper guess == map toUpper answer then
                putStrLn "Congratulations, you won!"
            else do
                putStrLn ("Guess " ++ show (length newHistory) ++ " of 6.")
                putStrLn ""
                gameLogic answer newHistory newKeyboardState


main :: IO ()
main = do
    putStrLn "--- Welcome to Haskell Wordle ---"
    --Takes in the file contents of "words.txt" and puts it into fileContent
    let file = "words.txt"
    fileContent <- readFile file
    --Makes a list and has each element as a line of the txt file
    -- Filters the list to only include words that are 5 letters long
    -- This is the word length for the game.
    let wordList = filter (\w -> length w == 5) (lines fileContent)
    --gets a random word and puts it into randomWord
    randomWord <- getRandomElement wordList
    gameLogic randomWord [] Map.empty 
-- The initial keyboard state and grid is empty, meaning no letters have been guessed yet.
-- The keyboard state will be updated as the game progresses.

    