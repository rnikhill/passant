{-# LANGUAGE OverloadedRecordDot #-}

module Chess
  ( getNextStates,
    printBoard,
    startGameState,
    startStateStr,
    mkGameState,
    BoardRep,
    GameState (_board),
  )
where

import Control.Monad (join)
import Data.Array as A
import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import Data.Int (Int8)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Prelude

-- import Debug.Trace (traceShow)

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq, Read, Show, Enum)

instance Hashable PieceType where
  hashWithSalt = hashUsing fromEnum

type BoardIndex = (Int8, Int8) -- (rank, file)

data Side = White | Black deriving (Eq, Read, Show, Enum)

instance Hashable Side where
  hashWithSalt = hashUsing fromEnum

other :: Side -> Side
other side = case side of
  White -> Black
  Black -> White

data Piece = Piece
  { _player :: Side,
    _pieceType :: PieceType,
    _hasMoved :: Bool
  }
  deriving (Eq, Read, Show)

instance Hashable Piece where
  hashWithSalt s piece = s `hashWithSalt` piece._player `hashWithSalt` piece._pieceType

data Square = Free | Filled Piece deriving (Eq, Read, Show)

instance Hashable Square where
  hashWithSalt s Free = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (Filled piece) = s `hashWithSalt` (1 :: Int) `hashWithSalt` piece

newtype Board = Board (A.Array BoardIndex Square) deriving (Eq, Read, Show)

instance Hashable Board where
  hashWithSalt s (Board board) =
    let squares = elems board
     in foldl' hashWithSalt s squares

type BoardRep = [[String]]

data GameState = GameState
  { _toPlay :: Side,
    _board :: Board,
    _drawMoveCounter :: Int,
    _prevBoard :: Maybe Board
  }
  deriving (Eq, Read, Show)

instance Hashable GameState where
  hashWithSalt s state =
    s `hashWithSalt` state._toPlay `hashWithSalt` state._board

data NormalMove = NormalMove
  { _start :: BoardIndex,
    _end :: BoardIndex,
    _capture :: Maybe Capture
  }
  deriving (Eq, Read, Show)

data CastleMove = Queenside | Kingside deriving (Eq, Read, Show)

data Move = Normal NormalMove | Castle CastleMove deriving (Eq, Read, Show)

data Capture = NormalCapture | EnPassantCapture deriving (Eq, Read, Show)

isSide :: Side -> Piece -> Bool
isSide side piece = side == piece._player

startStateStr :: BoardRep
startStateStr =
  [ ["bR", "bN", "bB", "bQ", "bK", "bB", "bN", "bR"],
    ["bP", "bP", "bP", "bP", "bP", "bP", "bP", "bP"],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["wP", "wP", "wP", "wP", "wP", "wP", "wP", "wP"],
    ["wR", "wN", "wB", "wQ", "wK", "wB", "wN", "wR"]
  ]

startGameState :: GameState
startGameState = mkGameState startStateStr

mkGameState :: BoardRep -> GameState
mkGameState boardRep = GameState {_toPlay = White, _board = buildBoard boardRep, _drawMoveCounter = 0, _prevBoard = Nothing}

readPieceType :: Char -> Maybe PieceType
readPieceType c = case c of
  'P' -> Just Pawn
  'R' -> Just Rook
  'N' -> Just Knight
  'B' -> Just Bishop
  'Q' -> Just Queen
  'K' -> Just King
  _ -> Nothing

readPiece :: String -> Maybe Piece
readPiece [p, s] = case (p, readPieceType s) of
  ('w', Just piece) -> Just $ Piece {_player = White, _pieceType = piece, _hasMoved = False}
  ('b', Just piece) -> Just $ Piece {_player = Black, _pieceType = piece, _hasMoved = False}
  _ -> Nothing
readPiece _ = Nothing

readSquare :: String -> Square
readSquare s = maybe Free Filled (readPiece s)

buildBoard :: BoardRep -> Board
buildBoard b = Board $ A.array ((1, 1), (8, 8)) $ zip ixs pieces
  where
    ixs = [(i, j) | i <- [8, 7 .. 1], j <- [1 .. 8]]
    pieces = map readSquare (join b)

printSquare :: Square -> String
printSquare square = case square of
  Free -> " "
  Filled Piece {_player = player, _pieceType = pieceType} ->
    let c = case pieceType of
          Pawn -> "p"
          Bishop -> "b"
          Knight -> "n"
          Rook -> "r"
          Queen -> "q"
          King -> "k"
     in if player == White then map toUpper c else c

printBoard :: Board -> String
printBoard (Board board_) =
  let rank idx = "| " ++ " | " `intercalate` [printSquare (board_ ! (idx, x)) | x <- [1 .. 8]] ++ " |"
      ranks = reverse [rank idx | idx <- [1 .. 8]]
      boardStr = "\n--------------------------------\n" `intercalate` ranks
   in boardStr

getNextStates :: [GameState] -> GameState -> [GameState]
getNextStates prevStates curState =
  let pieces = getPieceLocations curState._board
      pieceInfo = [(side, pieceType, ix) | (ix, Piece {_player = side, _pieceType = pieceType}) <- pieces]
      whitePieces = [(pT, ix) | (White, pT, ix) <- pieceInfo]
      blackPieces = [(pT, ix) | (Black, pT, ix) <- pieceInfo]
      curPieces = if curState._toPlay == White then whitePieces else blackPieces
      possibleMoves = curPieces >>= getMovesForPiece prevStates curState
   in map (applyMoveToState curState) possibleMoves

applyMoveToState :: GameState -> Move -> GameState
applyMoveToState curState move = case move of
  Normal normalMove -> applyNormalMove curState normalMove
  Castle _ -> curState

enPassantSquare :: BoardIndex -> BoardIndex -> BoardIndex
enPassantSquare (startRank, _) (_, endFile) = (startRank, endFile)

applyNormalMove :: GameState -> NormalMove -> GameState
applyNormalMove curState move =
  let (Board board) = curState._board
      oldSquare = board ! move._start
      newSquare = case oldSquare of
        Free -> error "Empty square selected as starting location for move"
        Filled piece -> Filled piece {_hasMoved = True}
      update = [(move._start, Free), (move._end, newSquare)]
      final = case move._capture of
        (Just EnPassantCapture) -> (enPassantSquare move._start move._end, Free) : update
        _ -> update
      newBoard = board // final
   in GameState {_toPlay = other curState._toPlay, _board = Board newBoard, _drawMoveCounter = curState._drawMoveCounter + 1, _prevBoard = Just curState._board}

getMovesForPiece :: [GameState] -> GameState -> (PieceType, BoardIndex) -> [Move]
getMovesForPiece prevStates gameState (pType, boardIndex) = case pType of
  Pawn -> getPawnMoves (listToMaybe prevStates) gameState boardIndex
  Bishop -> getBishopMoves gameState._board gameState._toPlay boardIndex
  Rook -> getRookMoves gameState._board gameState._toPlay boardIndex
  _ -> []

getPawnMoves :: Maybe GameState -> GameState -> BoardIndex -> [Move]
getPawnMoves prevState curState (rank, file) =
  getForwardPawnMoves curState (rank, file)
    ++ getPawnCaptures curState (rank, file)
    ++ case prevState of
      Nothing -> []
      (Just state) -> getEnPassant state._board curState (rank, file)

getForwardPawnMoves :: GameState -> BoardIndex -> [Move]
getForwardPawnMoves curState (rank, file) = getSingleSquareMove curState (rank, file) ++ getDoubleSquareMove curState (rank, file)

getSingleSquareMove :: GameState -> BoardIndex -> [Move]
getSingleSquareMove state ix@(rank, file) =
  let target =
        if state._toPlay == White
          then (rank + 1, file)
          else (rank - 1, file)
      canMoveTo = isValidIndex state._board target && isFree state._board target
   in ([Normal NormalMove {_start = ix, _end = target, _capture = Nothing} | canMoveTo])

getDoubleSquareMove :: GameState -> BoardIndex -> [Move]
getDoubleSquareMove state ix@(rank, file) =
  let target = if state._toPlay == White then (4, file) else (5, file)
      canMoveTo = (state._toPlay == White && rank == 2 || state._toPlay == Black && rank == 7) && isFree state._board target
   in ([Normal NormalMove {_start = ix, _end = target, _capture = Nothing} | canMoveTo])

getPawnCaptures :: GameState -> BoardIndex -> [Move]
getPawnCaptures state ix@(rank, file) =
  let targets = if state._toPlay == White then [(rank + 1, file + 1), (rank + 1, file - 1)] else [(rank - 1, file + 1), (rank - 1, file + 1)]
      valid = filter (isValidIndex state._board) targets
      otherSide = if state._toPlay == Black then White else Black
      enemyPieces = Set.fromList [ix' | (ix', _) <- filter (uncurry $ (flip $ const isSide) otherSide) (getPieceLocations state._board)]
      matches = filter (`Set.member` enemyPieces) valid
   in [Normal NormalMove {_start = ix, _end = ix', _capture = Just NormalCapture} | ix' <- matches]

getEnPassant :: Board -> GameState -> BoardIndex -> [Move]
getEnPassant prevBoard state (rank, file) =
  let otherSide = other state._toPlay
      hasOppPawn board ix = case getPieceAtSquare board ix of
        Just piece@Piece {_pieceType = Pawn} -> piece._player == otherSide
        _ -> False
      getStartingSquare (_, file') = case otherSide of
        White -> (2, file')
        Black -> (7, file')
      canCapture ix = hasOppPawn state._board ix && not (hasOppPawn state._board (getStartingSquare ix)) && not (hasOppPawn prevBoard ix) && hasOppPawn prevBoard (getStartingSquare ix)
      destSquares = filter canCapture [(rank, file - 1), (rank, file + 1)]
      mkMove (rank', file') = Normal NormalMove {_start = (rank, file), _end = (rank', file'), _capture = Just EnPassantCapture}
   in map mkMove destSquares

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

getBishopMoves :: Board -> Side -> BoardIndex -> [Move]
getBishopMoves board side startIx@(startRank, startFile) =
  let fwdRanks = [startRank + 1 ..]
      bwdRanks = [startRank - 1, startRank - 2 ..]
      rightFiles = [startFile + 1 ..]
      leftFiles = [startFile - 1, startFile - 2 ..]
      directions = [fwdRanks `zip` rightFiles, fwdRanks `zip` leftFiles, bwdRanks `zip` rightFiles, bwdRanks `zip` leftFiles]
   in getDirectionMoves board side startIx directions

getRookMoves :: Board -> Side -> BoardIndex -> [Move]
getRookMoves board side startIx@(startRank, startFile) =
  let fwd = [startRank + 1 ..]
      bwd = [startRank - 1, startRank - 2 ..]
      right = [startFile + 1 ..]
      left = [startFile - 1, startFile - 2 ..]
      directions = [fwd `zip` repeat startFile, bwd `zip` repeat startFile, repeat startRank `zip` right, repeat startRank `zip` left]
   in getDirectionMoves board side startIx directions

getDirectionMoves :: Board -> Side -> BoardIndex -> [[BoardIndex]] -> [Move]
getDirectionMoves board side startIx directions =
  let validDirections = map (takeWhile (isValidIndex board)) directions
      (free, taken) = unzip $ map (span (isFree board)) validDirections
      captures = [x | (Just x) <- map safeHead taken, hasPieceColor board (other side) x]
      captureMoves = [NormalMove {_start = startIx, _end = x, _capture = Just NormalCapture} | x <- captures]
      nonCaptureMoves = [NormalMove {_start = startIx, _end = x, _capture = Nothing} | x <- join free]
   in map Normal $ captureMoves ++ nonCaptureMoves

isValidIndex :: Board -> BoardIndex -> Bool
isValidIndex (Board arr) (rank, file) =
  let ((firstRank, firstFile), (lastRank, lastFile)) = A.bounds arr
   in firstRank <= rank && rank <= lastRank && firstFile <= file && file <= lastFile

isFree :: Board -> BoardIndex -> Bool
isFree (Board grid) space = grid ! space == Free

hasPieceColor :: Board -> Side -> BoardIndex -> Bool
hasPieceColor board@(Board boardArr) side ix
  | not $ isValidIndex board ix = False
  | otherwise = case boardArr ! ix of
      Free -> False
      Filled Piece {_player = player} -> side == player

getPieceAtSquare :: Board -> BoardIndex -> Maybe Piece
getPieceAtSquare board@(Board boardArr) ix
  | not $ isValidIndex board ix = Nothing
  | otherwise = case boardArr ! ix of
      Free -> Nothing
      Filled piece -> Just piece

getPieceLocations :: Board -> [(BoardIndex, Piece)]
getPieceLocations (Board arr) = [(ix, p) | (ix, Filled p) <- A.assocs arr]
