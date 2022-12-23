{-# LANGUAGE OverloadedRecordDot #-}

module Chess (startGameState, getNextStates) where

import Control.Monad (join)
import Data.Array as A
import Data.Int (Int8)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as Vec

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq, Read, Show)

type BoardIndex = (Int8, Int8) -- (rank, file)

type PieceID = Int8

data Side = White | Black deriving (Eq, Read, Show)

data Piece = Piece
  { player :: Side,
    pieceType :: PieceType,
    hasMoved :: Bool,
    pieceID :: PieceID
  }
  deriving (Eq, Read, Show)

data Square = Free | Filled Piece deriving (Eq, Read, Show)

newtype Board = Board (A.Array BoardIndex Square) deriving (Eq, Read, Show)

type BoardRep = [[String]]

newtype IDPosMap = IDPosMap (Vec.Vector PieceID) deriving (Eq, Read, Show)

data GameState = GameState
  { toPlay :: Side,
    board :: Board,
    numMoves :: Int,
    posByID :: IDPosMap
  }
  deriving (Eq, Read, Show)

data NormalMove = NormalMove
  { start :: BoardIndex,
    end :: BoardIndex,
    capture :: Maybe Capture
  }
  deriving (Eq, Read, Show)

data CastleMove = Queenside | Kingside deriving (Eq, Read, Show)

data Move = Normal NormalMove | Castle CastleMove deriving (Eq, Read, Show)

newtype NormalCapture = NormalCapture BoardIndex deriving (Eq, Read, Show)

data EnPassantCapture = EnPassantCapture
  { destination :: BoardIndex,
    captured :: BoardIndex
  }
  deriving (Eq, Read, Show)

data Capture = Regular NormalCapture | EnPassant EnPassantCapture deriving (Eq, Read, Show)

isSide :: Side -> Piece -> Bool
isSide side piece = side == piece.player

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

startState :: Board
startState = buildBoard (reverse startStateStr)

startGameState :: GameState
startGameState = GameState {toPlay = White, board = startState, numMoves = 0, posByID = IDPosMap Vec.empty}

readPieceType :: Char -> Maybe PieceType
readPieceType c = case c of
  'P' -> Just Pawn
  'R' -> Just Rook
  'N' -> Just Knight
  'B' -> Just Bishop
  'Q' -> Just Queen
  'K' -> Just King
  _ -> Nothing

readPiece :: String -> BoardIndex -> Maybe Piece
readPiece s (rank, file)
  | length s /= 2 = Nothing
  | otherwise = case (head s, readPieceType $ s !! 1) of
      ('w', Just p) -> Just $ Piece {player = White, pieceType = p, hasMoved = False, pieceID = rank * 8 + file}
      ('b', Just p) -> Just $ Piece {player = Black, pieceType = p, hasMoved = False, pieceID = rank * 8 + file - 32}
      _ -> Nothing

readSquare :: (String, BoardIndex) -> Square
readSquare (s, ix) = maybe Free Filled (readPiece s ix)

buildBoard :: BoardRep -> Board
buildBoard b = Board $ A.array ((1, 1), (8, 8)) $ zip ixs pieces
  where
    ixs = [(i, j) | i <- [8, 7 .. 1], j <- [1 .. 8]]
    pieces = zipWith (curry readSquare) (join b) ixs

getNextStates :: [GameState] -> GameState -> [GameState]
getNextStates prevStates curState =
  let pieces = getPieceLocations curState.board
      pieceInfo = [(side, pPieceType, ix) | (ix, Piece {player = side, pieceType = pPieceType}) <- pieces]
      whitePieces = [(pT, ix) | (White, pT, ix) <- pieceInfo]
      blackPieces = [(pT, ix) | (Black, pT, ix) <- pieceInfo]
      curPieces = if curState.toPlay == White then whitePieces else blackPieces
      possibleMoves = curPieces >>= getMovesForPiece prevStates curState curState.toPlay
   in map (applyMoveToState prevStates curState) possibleMoves

applyMoveToState :: [GameState] -> GameState -> Move -> GameState
applyMoveToState prevStates curState move
  | null prevStates = error "hello world"
  | curState.toPlay == White = error "hello world"
  | otherwise = case move of
      Normal _ -> error "hello world"
      Castle _ -> error "hello world"

getMovesForPiece :: [GameState] -> GameState -> Side -> (PieceType, BoardIndex) -> [Move]
getMovesForPiece prevStates curState@GameState {board = chessboard} _ (pType, boardIndex) = case pType of
  Pawn -> getPawnMoves (listToMaybe prevStates) curState boardIndex
  Bishop -> getBishopMoves chessboard boardIndex
  _ -> undefined

getPawnMoves :: Maybe prevState -> GameState -> BoardIndex -> [Move]
getPawnMoves _ curState (rank, file) =
  getForwardPawnMoves curState (rank, file)
    ++ getPawnCaptures curState (rank, file)
    ++ getEnPassant curState (rank, file)

getForwardPawnMoves :: GameState -> BoardIndex -> [Move]
getForwardPawnMoves curState (rank, file) = getSingleSquareMove curState (rank, file) ++ getDoubleSquareMove curState (rank, file)

getSingleSquareMove :: GameState -> BoardIndex -> [Move]
getSingleSquareMove state ix@(rank, file) =
  let target =
        if state.toPlay == White
          then (rank + 1, file)
          else (rank - 1, file)
      canMoveTo = isValidIndex state.board target && isFree state.board target
   in ([Normal NormalMove {start = ix, end = target, capture = Nothing} | canMoveTo])

getDoubleSquareMove :: GameState -> BoardIndex -> [Move]
getDoubleSquareMove state ix@(rank, file) =
  let target = if state.toPlay == White then (4, file) else (5, file)
      canMoveTo = isValidIndex state.board target && isFree state.board target && (state.toPlay == White && rank == 2 || state.toPlay == Black && rank == 7)
   in ([Normal NormalMove {start = ix, end = target, capture = Nothing} | canMoveTo])

getPawnCaptures :: GameState -> BoardIndex -> [Move]
getPawnCaptures state ix@(rank, file) =
  let targets = if state.toPlay == White then [(rank + 1, file + 1), (rank + 1, file - 1)] else [(rank - 1, file + 1), (rank - 1, file + 1)]
      valid = filter (isValidIndex state.board) targets
      otherSide = if state.toPlay == Black then White else Black
      enemyPieces = Set.fromList [ix' | (ix', _) <- filter (uncurry $ (flip $ const isSide) otherSide) (getPieceLocations state.board)]
      matches = filter (`Set.member` enemyPieces) valid
   in [Normal NormalMove {start = ix, end = ix', capture = Just (Regular (NormalCapture ix'))} | ix' <- matches]

getEnPassant :: GameState -> BoardIndex -> [Move]
getEnPassant = undefined

getBishopMoves :: Board -> BoardIndex -> [Move]
getBishopMoves = undefined

isValidIndex :: Board -> BoardIndex -> Bool
isValidIndex (Board arr) (rank, file) =
  let ((firstRank, firstFile), (lastRank, lastFile)) = A.bounds arr
   in firstRank <= rank && rank <= lastRank && file <= firstFile && file <= lastFile

isFree :: Board -> BoardIndex -> Bool
isFree (Board grid) space = grid A.! space == Free

getPieceLocations :: Board -> [(BoardIndex, Piece)]
getPieceLocations (Board arr) = [(ix, p) | (ix, Filled p) <- A.assocs arr]

{-

getPiecePosByID :: IDPosMap -> PieceID -> Maybe BoardIndex
getPiecePosByID = undefined

getPieceByID :: GameState -> PieceID -> Maybe (Piece, BoardIndex)
getPieceByID = undefined
-}