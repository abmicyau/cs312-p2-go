-- Go in Haskell
--
-- CPSC 312
-- 2017W1 Project 2

-- TODO:
--
-- [DONE] 1) If the move string is invalid, try again
-- [DONE] 2) Attempt to apply move to board:
  -- Check for illegal move - cases:
    -- [DONE] suicide (requires creating a new board and checking its state)
    -- [DONE] out of bounds (looking at the coordinates should be enough)
    -- [DONE] already occupied (looking at the coordinates should be enough)
    -- ko limit (optional? need to keep track of repeated moves)
  -- [DONE] If illegal, then get another move from the player
-- 3) If the move is not illegal, then:
    -- [DONE] Apply the move to the board to create a new board
    -- Check the new board for new captures and create another new board
      -- A group of connected stones is captured if no stone in the group
      -- has an empty neighbour (no liberties)
    -- Update scores
    -- [DONE] Call gameLoop with the new board and switch players
-- [DONE] 4) Make a createGameBoard function that can create any sized NxN board
-- [DONE] 5) Improve board printing (using an actual grid labeled with numbers for rows and cols)
-- 6) Add some sort of indicator for the star points on the board

import Data.List
import Data.Char
import Data.Maybe

-- Main function. Just calls gameLoop for now.
main :: IO b
main = do
  putStrLn $ replicate 2 '\n'
  putStrLn "Welcome to GO!\n"
  gameLoop (generateBoard 19) 'b' (0,0) False

type Board = [[Char]]
type Move = (Int, Int)
-- Score is (black, white). For example (12, 5)
-- means black has 12 points and white has 5 points
type Score = (Int, Int)

-- Main game loop
--  board: game board (NxN matrix)
--  playerChar: either 'b' or 'w' indicating whose turn it is
--  singlePlayer: boolean flag for AI (not implemented yet)
--
gameLoop :: Board -> Char -> Score -> Bool -> IO b
gameLoop board player score singlePlayer = do
    if singlePlayer && player == 'b' then do
      -- AI's turn
      let move = aiMove board
      -- AI move guaranteed to be legal
      let board2 = fromJust $ doMove board move player
      let (board3, newScore) = capture board2 score
      gameLoop board3 (nextPlayer player) newScore singlePlayer
    else do
      printBoard board
      putStrLn $ replicate 1 '\n'
      putStrLn $ "Playing: " ++ toPlayerText player ++ " - please enter a move: "
      putStrLn $ "Type 'a b' (without quotes) to place a stone in row a, column b.\n"
      line <- getLine
      putStrLn $ replicate 1 '\n'
      let maybeMove = parseMove line
      if isJust maybeMove then do
        -- valid input
        let move = fromJust maybeMove
        let maybeBoard2 = doMove board move player
        if isJust maybeBoard2 then do
          -- possibly legal move (not out of bounds, not occupied)
          -- ... BUT need to check for suicide!

          let board2 = fromJust maybeBoard2
          let (board3, newScore) = capture board2 score

          -- check newBoard for suicide
          if isOccupied board3 move then do
            -- legal move
            gameLoop board3 (nextPlayer player) newScore singlePlayer
          else do
            -- illegal move (suicide)
            gameLoop board player score singlePlayer
        else do
          -- illegal move (out of bounds or already occupied)
          gameLoop board player score singlePlayer
      else do
        -- invalid input
        gameLoop board player score singlePlayer

-- Parses a move string (for example, "5 12") into an optional tuple containing the
-- coordinates as integers, otherwise Nothing if there is any parsing error or ambiguity
-- For example:
-- parsemove "5 12"
-- > Just (5, 12)
-- parseMove "abcd"
-- > Nothing
--
parseMove :: String -> Maybe Move
parseMove move
  | (length coords == 2) && (length rowTupleArr == 1) && (length $ snd $ rowTupleArr !! 0) == 0 &&
    (length colTupleArr == 1) && (length $ snd $ colTupleArr !! 0) == 0 =
    Just (fst $ rowTupleArr !! 0, fst $ colTupleArr !! 0)
  | otherwise = Nothing
  where
    coords = words move
    rowTupleArr = reads $ coords !! 0 :: [(Int, String)]
    colTupleArr = reads $ coords !! 1 :: [(Int, String)]

doMove :: Board -> Move -> Char -> Maybe Board
doMove board move player
 | row < 1 || col < 1 || row > size || col > size = Nothing -- Out of bounds
 | isOccupied board move = Nothing -- Already occupied
 | otherwise = Just $ putStone board move player
 where
  row = fst move
  col = snd move
  size = length board

-- NOTE: Assumes that move is NOT out of bounds (is within the board size)
isOccupied :: Board -> Move -> Bool
isOccupied board move
 | stone == 'b' || stone == 'w' = True
 | otherwise = False
 where
  row = fst move
  boardRow = board!!(row-1)
  col = snd move
  stone = boardRow!!(col-1)

-- NOTE: Assumes that the move is NOT out of bounds
putStone :: Board -> Move -> Char -> Board
putStone board move player =
  take (row-1) board ++
    [take (col-1) boardRow ++ [player] ++ drop col boardRow] ++
    drop row board
  where
    row = fst move
    boardRow = board!!(row-1)
    col = snd move

generateBoard :: Int -> Board
generateBoard size = replicate size $ replicate size ' '

-- Checks the entire board for new captures and creates a new board
-- and set of scores with the updated state.
--
-- TODO
--
capture :: Board -> Score -> (Board, Score)
capture board score = (board, score)







-- Other Helpers

-- Given a player ('b' or 'w'), returns the next player
-- For example:
-- nextPlayer 'w'
-- > 'b'
-- nextPlayer 'b'
-- > 'w'
--
nextPlayer :: Char -> Char
nextPlayer p = if p == 'w' then 'b' else 'w'

-- Converts a player char ('b' or 'w') to the string representation
-- For example:
-- toPlayerText 'w'
-- > "WHITE"
-- toPlayerText 'b'
-- > "BLACK"
--
toPlayerText :: Char -> String
toPlayerText p = if p == 'w' then "WHITE" else "BLACK"

-- Pads an integer to the specified length with spaces
-- For example:
-- padSpaces 2 2
-- > " 2"
-- padSpaces 0 4
-- > "4"
-- padSpaces 4 13
-- > "  13"
--
padSpaces :: Int -> Int -> String
padSpaces n v = replicate (n - length str) ' ' ++ str
  where str = show v

repeatString :: Int -> String -> String
repeatString n str = if n <= 0 then "" else str ++ repeatString (n-1) str

--
printBoard :: Board -> IO ()
printBoard board = do
  printHeader 1
  printBoardHelper board 1

  where
    size = length board

    printBoardHelper :: Board -> Int -> IO ()
    printBoardHelper board n = do
      printRow (board!!0) n
      if n < size then do
        printVBars size
        printBoardHelper (drop 1 board) (n+1)
      else return ()

    printRow :: String -> Int -> IO ()
    printRow row n = do
      if length row == size then do
        putStr $ padSpaces 2 n ++ " " ++ [charToStone n 1 (row!!0)]
        printRow (drop 1 row) n
      else if length row > 1 then do
        putStr $ "\x2500\x2500\x2500" ++ [charToStone n (size - (length row) + 1) (row!!0)]
        printRow (drop 1 row) n
      else
        putStr $ "\x2500\x2500\x2500" ++ [charToStone n size (row!!0)] ++ "\n"

    charToStone :: Int -> Int -> Char -> Char
    charToStone row col char =
      if char == 'w' then '\x25cb'
      else if char == 'b' then '\x25cf'
      else if row == 1 then
        if col == 1 then '\x250c'
        else if col < size then '\x252c'
        else '\x2510'
      else if row < size then
        if col == 1 then '\x251c'
        else if col < size then '\x253c'
        else '\x2524'
      else
        if col == 1 then '\x2514'
        else if col < size then '\x2534'
        else '\x2518'

    printHeader :: Int -> IO ()
    printHeader n = do
      if n <= size then do
        putStr $ padSpaces 4 n
        printHeader $ n + 1
      else
        putStr "\n"

    printVBars :: Int -> IO ()
    printVBars n = putStr $ repeatString n "   \x2502" ++ "\n"


-- AI --

-- Let's just say the AI is always black
aiMove :: Board -> Move
aiMove board = (0,0)