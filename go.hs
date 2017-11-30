-- Go in Haskell
--
-- CPSC 312
-- 2017W1 Project 2

-- TODO:
--
-- [DONE] 1) If the move string is invalid, try again
-- [DONE] 2) Attempt to apply move to board:
  -- Check for illegal move - cases:
    -- suicide (requires creating a new board and checking its state)
    -- [DONE] out of bounds (looking at the coordinates should be enough)
    -- [DONE] already occupied (looking at the coordinates should be enough)
    -- ko limit (optional? need to keep track of repeated moves)
  -- [DONE] If illegal, then get another move from the player
-- 3) If the move is not illegal, then:
    -- [DONE] Apply the move to the board to create a new board
    -- Check the new board for new captures and create another new board
      -- A group of connected stones is captured if no stone in the group
      -- has an empty neighbour
    -- Update scores
    -- Call gameLoop with the new board and switch players
-- 4) Make a createGameBoard function that can create any sized NxN board
-- 5) Improve board printing (using an actual grid labeled with numbers for rows and cols)

import Data.List
import Data.Char
import Data.Maybe

-- Main function. Just calls gameLoop for now.
main :: IO b
main = do
  putStrLn $ take 2 $ repeat '\n'
  putStrLn "Welcome to GO!\n"
  gameLoop emptyBoard 'b' (0,0) True

type Board = [[Char]]
type Move = (Int, Int)
-- Score is (black, white). For example (12, 5)
-- means black has 12 points and white has 5 points
type Score = (Int, Int)

-- 9x9 sample board
--  'w' for a white stone
--  'b' for a black stone
--  ' ' for an empty space
--
emptyBoard = ["         ",
              "         ",
              "         ",
              "         ",
              "         ",
              "         ",
              "         ",
              "         ",
              "         "]

-- Main game loop
--  board: game board (NxN matrix)
--  playerChar: either 'b' or 'w' indicating whose turn it is
--  singlePlayer: boolean flag for AI (not implemented yet)
--
gameLoop :: Board -> Char -> Score -> Bool -> IO b
gameLoop board player score singlePlayer = do
    printBoard board
    putStrLn $ take 1 $ repeat '\n'
    putStrLn $ "Playing: " ++ toPlayerText player ++ " - please enter a move: "
    line <- getLine
    putStrLn $ take 1 $ repeat '\n'
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


--
printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard (h:t) = do
    printRow h
    printBoard t
    where
      printRow [] = putStr "\n"
      printRow (h:t) = do
        if h == 'w' then
          putStr "\x25CB "
        else if h == 'b' then
          putStr "\x25CF "
        else
          putStr "- "
        printRow t

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

-- Checks the entire board for new captures and creates a new board
-- and set of scores with the updated state.
--
-- TODO
--
capture :: Board -> Score -> (Board, Score)
capture board score = (board, score)







-- Other Helpers

-- Given a player ('b' or 'w'), returns the next player
--
nextPlayer :: Char -> Char
nextPlayer p = if p == 'w' then 'b' else 'w'

toPlayerText :: Char -> String
toPlayerText p = if p == 'w' then "WHITE" else "BLACK"