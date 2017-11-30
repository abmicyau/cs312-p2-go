-- Go in Haskell
--
-- CPSC 312
-- 2017W1 Project 2

import Data.List
import Data.Char

-- Main function. Just calls gameLoop for now.
--
main = do
  putStrLn $ take 2 $ repeat '\n'
  putStrLn "Welcome to GO!\n"
  gameLoop emptyBoard 'b' True

-- 5x5 sample board
--  'w' for a white stone
--  'b' for a black stone
--  ' ' for an empty space
-- TODO:
-- 1) Make a createGameBoard function that can create any sized NxN board
--
emptyBoard = ["   w ",
              " b   ",
              "     ",
              "  b  ",
              " w   "]

-- Main game loop
--  board: game board (NxN matrix)
--  playerChar: either 'b' or 'w' indicating whose turn it is
--  singlePlayer: boolean flag for AI (not currently used)
--
gameLoop :: [[Char]] -> Char -> t -> IO b
gameLoop board playerChar singlePlayer = do
    printBoard board
    putStrLn $ "It's player " ++ [playerChar] ++ "'s turn - please enter a move: "
    line <- getLine
    putStrLn $ take 2 $ repeat '\n'
    let move = parseMove line
    -- TODO:
    -- 1) If the move string is invalid, try again
    -- 2) Attempt to apply move to board:
      -- Check for illegal move - cases:
        -- suicide (requires creating a new board and checking its state)
        -- out of bounds (looking at the coordinates should be enough)
        -- already occupied (looking at the coordinates should be enough)
        -- ko limit (optional? need to keep track of repeated moves)
      -- If illegal, then get another move from the player
    -- 3) If the move is not illegal, then:
        -- Apply the move to the board to create a new board
        -- Check the new board for new captures and create another new board
          -- A group of connected stones is captured if no stone in the group
          -- has an empty neighbour
        -- Update scores
        -- Call gameLoop with the new board and switch players
    gameLoop board playerChar singlePlayer

-- TODO:
-- 1) Make this look better (using an actual grid labeled with numbers for rows and cols)
--
printBoard :: [[Char]] -> IO ()
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

-- Parses a move string (for example, "5 12") into a tuple containing a tuple with the
-- coordinates as integers, and whether the move is valid (ignoring legal board positions)
-- For example:
-- parsemove "5 12"
-- > ((5, 12), True)
-- parseMove "abcd"
-- > ((0, 0), False)
--
parseMove :: String -> ((Int, Int), Bool)
parseMove move
  | (length coords == 2) && (length rowTupleArr == 1) && (length $ snd $ rowTupleArr !! 0) == 0 &&
    (length colTupleArr == 1) && (length $ snd $ colTupleArr !! 0) == 0 =
    ((fst $ rowTupleArr !! 0, fst $ colTupleArr !! 0), True)
  | otherwise = invalidMove
  where
    invalidMove = ((0,0), False)
    coords = words move
    rowTupleArr = reads $ coords !! 0 :: [(Int, String)]
    colTupleArr = reads $ coords !! 1 :: [(Int, String)]