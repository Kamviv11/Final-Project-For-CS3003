# Haskell Wordle

A command-line implementation of the popular Wordle game, written in Haskell.  
This version features a user-friendly display of guesses and a live keyboard tracker to help visualize which letters have been guessed and their states.

## Features

- Randomly selects a 5-letter word from a word list.
- Accepts user guesses and provides colored feedback:
  - 🟩 Correct letter, correct position
  - 🟨 Correct letter, wrong position
  - 🟥 Incorrect letter
  - 🟦 Unguessed/unused letter
- Displays a virtual keyboard showing the state of each letter.
- Tracks and displays all previous guesses.
- Case-insensitive input.

## How to Play

1. Run the program.
2. Enter your 5-letter guess when prompted.
3. Use the colored feedback and keyboard tracker to inform your next guess.
4. You have 6 attempts to guess the correct word.

## Setup & Running

1. **Install GHC (The Glasgow Haskell Compiler)**  
   [Download GHC](https://www.haskell.org/ghc/) or use [Stack](https://docs.haskellstack.org/en/stable/README/).

2. **Clone this repository and navigate to the project folder:**
   ```sh
   git clone <your-repo-url>
   cd Wordle-w-Haskell/Final_Project/app
   ```

3. **Ensure you have a `words.txt` file**  
   Place a file named `words.txt` in the same directory as `Main.hs`.  
   Each line should be a 5-letter word.

4. **Build and run:**
   ```sh
   ghc Main.hs -o wordle
   ./wordle
   ```
   Or, if using Stack:
   ```sh
   stack ghc -- Main.hs -o wordle
   ./wordle
   ```

## File Structure

- `Main.hs` — Main game logic and UI.
- `words.txt` — List of valid 5-letter words (one per line).

## Example

```
--- Welcome to Haskell Wordle ---
-----Keyboard State-----
🟦Q🟦 🟦W🟦 🟦E🟦 🟦R🟦 🟦T🟦 🟦Y🟦 🟦U🟦 🟦I🟦 🟦O🟦 🟦P🟦
🟦A🟦 🟦S🟦 🟦D🟦 🟦F🟦 🟦G🟦 🟦H🟦 🟦J🟦 🟦K🟦 🟦L🟦
🟦Z🟦 🟦X🟦 🟦C🟦 🟦V🟦 🟦B🟦 🟦N🟦 🟦M🟦
----------------------

Enter your guess (5 letters)
```

## License

MIT License

---

Enjoy playing Wordle in your terminal, powered by Haskell!

KEY NOTE: 
Base Implementation by Sai Vivek 
Extended Improvements by Jonathan Cheruiyot
