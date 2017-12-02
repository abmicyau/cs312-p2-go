-- Go in Haskell
--
-- CPSC 312
-- 2017W1 Project 2

import Data.List
import Data.Char
import Data.Maybe
import Data.Time.Clock.POSIX

-- Main function. Just calls gameLoop for now.
main :: IO b
main = do
  putStrLn $ replicate 2 '\n'
  putStrLn "Welcome to GO!\n"
  gameLoop (generateBoard 19) 'b' (0,0) False

-- Board is a matrix of characters
-- 'w' is white
-- 'b' is black
-- ' ' is an empty space
--
type Board = [[Char]]

-- Each move is a tuple containing the row and column
--
type Move = (Int, Int)

-- Score is (black, white). For example (12, 5)
-- means black has 12 points and white has 5 points
--
type Score = (Int, Int)

-- Main game loop
--  board: game board (NxN matrix)
--  playerChar: either 'b' or 'w' indicating whose turn it is
--  singlePlayer: boolean flag for AI
--
gameLoop :: Board -> Char -> Score -> Bool -> IO b
gameLoop board player score singlePlayer = do
  if singlePlayer && player == 'b' then do
    -- AI's turn
    move <- aiMove board player
    -- AI move guaranteed to be legal
    let board2 = fromJust $ doMove board move player
    let (board3, newScore) = capture board2 score move
    gameLoop board3 (nextPlayer player) newScore singlePlayer
  else do
    printBoard board
    putStrLn $ replicate 1 '\n'
    putStrLn $ "Type 'a b' (without quotes) to place a stone in row a, column b."
    putStrLn $ "To skip turn, type 0 0.\n"
    putStrLn $ "Playing: " ++ toPlayerText player ++ " - please enter a move: "
    line <- getLine
    putStrLn $ replicate 1 '\n'
    let maybeMove = parseMove line
    if isJust maybeMove then do
      -- valid input
      let move = fromJust maybeMove
      if (fst move) == 0 && (snd move) == 0 then do
        gameLoop board (nextPlayer player) score singlePlayer
      else do
        let maybeBoard2 = doMove board move player
        if isJust maybeBoard2 then do
          -- possibly legal move (not out of bounds, not occupied)
          -- ... BUT need to check for suicide!
          let board2 = fromJust maybeBoard2
          let (board3, newScore) = capture board2 score move

          -- check newBoard for suicide
          if isSuicide board3 move then do
            -- do not allow player to make suicide move
            gameLoop board player score singlePlayer
          else do
            -- legal move
            gameLoop board3 (nextPlayer player) newScore singlePlayer
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

-- Performs a move on a board for a specified player and generates a new board
--
doMove :: Board -> Move -> Char -> Maybe Board
doMove board move player
  | row < 1 || col < 1 || row > size || col > size = Nothing -- Out of bounds
  | isOccupied board move = Nothing -- Already occupied
  | otherwise = Just $ putStone board move player
  where
    row = fst move
    col = snd move
    size = length board

-- Checks whether the board has a piece located at a specified position
-- NOTE: Assumes that the move is NOT out of bounds (is within the board size)
--
isOccupied :: Board -> Move -> Bool
isOccupied board move
 | stone == 'b' || stone == 'w' = True
 | otherwise = False
 where
  row = fst move
  boardRow = board!!(row-1)
  col = snd move
  stone = boardRow!!(col-1)

-- Places a stone at a position on a board to generate a new board
-- NOTE: Assumes that the move is NOT out of bounds
--
putStone :: Board -> Move -> Char -> Board
putStone board move player =
  take (row-1) board ++
    [take (col-1) boardRow ++ [player] ++ drop col boardRow] ++
    drop row board
  where
    row = fst move
    boardRow = board!!(row-1)
    col = snd move

-- Generates an NxN board
--
generateBoard :: Int -> Board
generateBoard size = replicate size $ replicate size ' '

-- Generate a random integer between 0 and n-1 inclusive
--
randInt :: Int -> IO Int
randInt n = do
  ms <- (fmap (\ x -> round (x * 1000000)) getPOSIXTime)
  return $ mod ms n

-- Takes a list and returns a random element and the list with the element removed
-- NOTE: Assumes the list is NON-EMPTY.
--
sample :: [a] -> IO (a,[a])
sample lst = do
  let len = length lst
  index <- randInt len
  let element = lst!!index
  let slice = take index lst ++ drop (index+1) lst
  return (element, slice)

-- Samples n random elements from a list, or the entire list of n is greater than
-- or equal to the length of the list
-- NOTE: Assumes n >= 0
--
sampleN :: [a] -> Int -> IO [a]
sampleN lst n = do
  let len = length lst
  if n == 0 || len == 0 then return []
  else do
    (element, slice) <- sample lst
    rest <- sampleN slice (n-1)
    return $ element : rest

-- Returns the index of the largest positive element in a list or 0 otherwise
--
indexMax :: [Int] -> Int
indexMax lst =
  indexMaxHelper lst 0 0 0
  where
    indexMaxHelper lst cur idx max
      | length lst == 0 = idx
      | otherwise =
        if lst!!0 > max then
          indexMaxHelper (drop 1 lst) (cur+1) cur (lst!!0)
        else
          indexMaxHelper (drop 1 lst) (cur+1) idx max

-- Simulates a move on a board at a specified position and performs all possible
-- captures from that position to generate a new board
--
capture :: Board -> Score -> Move -> (Board, Score)
capture board score move = do
  let board1 = removePieces board leftPieces
      board2 = removePieces board1 upPieces
      board3 = removePieces board2 rightPieces
      board4 = removePieces board3 downPieces
      allPieces = leftGroup ++ upGroup ++ rightGroup ++ downGroup
      newScore = updateScore score (length allPieces) player
      in (board4, newScore)
  where
    player = getPiece board move
    left = (fst move, snd move-1)
    right = (fst move, snd move+1)
    up = (fst move-1, snd move)
    down = (fst move+1, snd move)
    size = length board
    leftGroup =
      if not (isInBounds size left)
        then []
        else if player == getPiece board left
          then []
          else getGroup board left left []
    upGroup =
      if not (isInBounds size up)
        then []
        else if player == getPiece board up
          then []
          else if (up `elem` leftGroup)
            then []
            else getGroup board up up []
    rightGroup =
      if not (isInBounds size right)
        then []
        else if player == getPiece board right
          then []
          else if (right `elem` leftGroup) || (right `elem` upGroup)
            then []
            else getGroup board right right []
    downGroup =
      if not (isInBounds size down)
        then []
        else if player == getPiece board down
          then []
          else if (down `elem` leftGroup) || (down `elem` upGroup) || (down `elem` rightGroup)
            then []
            else getGroup board down down []
    leftPieces =
      if (isDead board leftGroup)
        then leftGroup
        else []
    upPieces =
      if (isDead board upGroup)
        then upGroup
        else []
    rightPieces =
      if (isDead board rightGroup)
        then rightGroup
        else []
    downPieces =
      if (isDead board downGroup)
        then downGroup
        else []

-- Generate new board with pieces in given list removed from board
--
removePieces :: Board -> [Move] -> Board
removePieces board [] = board
removePieces board (h:t) = removePieces (putStone board h ' ') t

-- Update score by counting number of captured pieces and adding to the
-- appropriate player
updateScore :: Score -> Int -> Char -> Score
updateScore score size player =
  if player == 'B'
    then (fst score + size, snd score)
    else (fst score, snd score + size)

-- get all tiles of same color from starting tile
--
getGroup :: Board -> Move -> Move -> [Move] -> [Move]
getGroup board position prev group
  | not (isInBounds size position) = group
  | position `elem` group = group
  | getPiece board position /= getPiece board prev = group
  | otherwise =
    let list1 = getGroup board left position group++[position]
        list2 = getGroup board right position list1
        list3 = getGroup board up position list2
        list4 = getGroup board down position list3
        in list4
  where
    row = fst position
    col = snd position
    size = length board
    left = (row, col-1)
    right = (row, col+1)
    up = (row-1, col)
    down = (row+1, col)

-- checks if a move results in a suicide
-- assumes move is valid
--
isSuicide :: Board -> Move -> Bool
isSuicide board move = isDead board group
  where
    group = getGroup board move move []

-- checks if a group of tiles of the same color should be killed
-- a group is killable if it is surrounded by only hostile tiles
-- or non-inbounds tiles
isDead :: Board -> [Move] -> Bool
-- if we've processed the entire list then group should be killed
isDead board [] = True
isDead board (h:t)
  | not (isSurrounded board h) = False
  | otherwise = isDead board t

-- Checks whether or not a given piece has any liberties
--
isSurrounded :: Board -> Move -> Bool
isSurrounded board piece = leftHostile && upHostile && rightHostile && downHostile
  where
    row = fst piece
    col = snd piece
    size = length board
    left = (row, col-1)
    right = (row, col+1)
    up = (row-1, col)
    down = (row+1, col)
    leftHostile =
      if not (isInBounds size left)
        then True
        else if not (isOccupied board left)
          then False
          else True
    rightHostile =
      if not (isInBounds size right)
        then True
        else if not (isOccupied board right)
          then False
          else True
    upHostile =
      if not (isInBounds size up)
        then True
        else if not (isOccupied board up)
          then False
          else True
    downHostile =
      if not (isInBounds size down)
        then True
        else if not (isOccupied board down)
          then False
          else True

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

-- Repeats a string n times
--
repeatString :: Int -> String -> String
repeatString n str = if n <= 0 then "" else str ++ repeatString (n-1) str

-- return the value of the piece on the board at move's position
-- move is (row, col), assume move is within board
-- piece can be one of: 'b', 'w', or "\n"
getPiece :: Board -> Move -> Char
getPiece board position = boardPiece
  where
    row = fst position
    boardRow = board!!(row-1)
    col = snd position
    boardPiece = boardRow!!(col-1)

-- checks if a piece is in bounds of the game board given board size
--
isInBounds :: Int -> Move -> Bool
isInBounds size position
  | row < 1 || row > size || col < 1 || col > size = False
  | otherwise = True
  where
    row = fst position
    col = snd position

-- Prints the board
--
printBoard :: Board -> IO ()
printBoard board = do
  printHeader 1
  printBoardHelper board 1
  printHeader 1

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
        putStr $ "\x2500\x2500\x2500" ++ [charToStone n size (row!!0)] ++ " " ++ padSpaces 2 n ++ "\n"

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

-- For simplicity, we make the following assumptions for the AI:
--  - We are always playing on a 19x19 board
--  - Corner (star) and adjacent positions have the highest priority
--  - If all corner positions are taken, attempt to make an L or diagonal
--  - Points between star positions get the next highest priority
--  - Otherwise base move on a heatmap of scores (see baseScore) plus
--    any additional scores from capturing stones
--
aiMove :: Board -> Char -> IO Move
aiMove board player = do
  -- 1) Generate a list of all possible legal moves on the board
  allMoves <- getAllLegalMoves board player

  -- 2) Randomly sample 16 different legal moves
  randomSample <- sampleN allMoves 16

  -- 3) Pick the move with the highest score
  let move = bestMove randomSample board player

  return move

-- Checks whether a move is legal
--
isLegalMove :: Board -> Move -> Char -> IO Bool
isLegalMove board move player = do
  let maybeBoard = doMove board move player
  if isJust maybeBoard then do
    let board2 = fromJust maybeBoard
    if not $ isSuicide board2 move then
      return True
    else return False
  else return False

-- Given a board, generates a list of all possible legal moves that can
-- be played on that board
--
getAllLegalMoves :: Board -> Char -> IO [Move]
getAllLegalMoves board player = do
  result <- helper board player 1 1
  return result
  where
    size = length board
    helper board player row col = do
      legalMove <- isLegalMove board (row,col) player
      if row > size then do
        return []
      else if col == size then do
        if legalMove then do
          result <- helper board player nextRow 1
          return $ (row,col):result
        else do
          result <- helper board player nextRow 1
          return result
      else do
        if legalMove then do
          result <- helper board player row nextCol
          return $ (row,col):result
        else do
          result <- helper board player row nextCol
          return result
      where
        nextRow = row+1
        nextCol = col+1

-- Given a list of moves, returns the move that would result in the largest score
-- based on the following score function:
--  score = row score + column score + (# pieces captured * 16)
--
bestMove :: [Move] -> Board -> Char -> Move
bestMove lst board player =
  lst!!index
  where
    captureScore move
      | player == 'b' = fst $ snd $ capture board (0,0) move
      | otherwise = snd $ snd $ capture board (0,0) move
    moveToScore move =
      (baseScore $ fst move) + (baseScore $ snd move) + (captureScore move * 16)
    scores = map (\x -> moveToScore x) lst
    index = indexMax scores

-- Convert a row or column number into a base score, based on the
-- following pattern on a 19x19 board (same with row or column):
-- 0 2 4 6 5 4 3 2 1 0 1 2 3 4 5 6 4 2 0
--
baseScore :: Int -> Int
baseScore n
  | n < 4 = (n-1)*2
  | n > 16 = (19-n)*2
  | n < 10 = 10-n
  | n > 10 = n-10
  | otherwise = 0
