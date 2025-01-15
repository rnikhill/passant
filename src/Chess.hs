{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Chess
  ( getNextStates,
    printBoard,
    startGameState,
    startStateStr,
    mkGameState,
    BoardRep,
    GameState (..),
    Side (..),
    PieceType (..),
    kingIsInCheck,
    Piece (..),
    getPieceLocations,
  )
where

import Control.Monad (join)
import Data.Array as A
import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import Data.Int (Int8)
import Data.List (intercalate, partition)
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
    _board :: !Board,
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
    _capture :: Maybe Capture,
    _promote :: Maybe PieceType
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

getNextStates :: GameState -> [GameState]
getNextStates curState =
  let pieces = getPieceLocations curState._board
      pieceInfo = [(side, pieceType, ix) | (ix, Piece {_player = side, _pieceType = pieceType}) <- pieces]
      whitePieces = [(pT, ix) | (White, pT, ix) <- pieceInfo]
      blackPieces = [(pT, ix) | (Black, pT, ix) <- pieceInfo]
      curPieces = if curState._toPlay == White then whitePieces else blackPieces
      possibleMoves = curPieces >>= getMovesForPiece True curState
   in map (applyMoveToState curState) possibleMoves

applyMoveToState :: GameState -> Move -> GameState
applyMoveToState curState move = case move of
  Normal normalMove -> applyNormalMove curState normalMove
  Castle castleMove -> applyCastleMove curState castleMove

enPassantSquare :: BoardIndex -> BoardIndex -> BoardIndex
enPassantSquare (startRank, _) (_, endFile) = (startRank, endFile)

applyNormalMove :: GameState -> NormalMove -> GameState
applyNormalMove curState move =
  let (Board board) = curState._board
      oldSquare = board ! move._start
      newSquare = case oldSquare of
        Free -> error "Empty square selected as starting location for move"
        Filled piece -> case move._promote of
          Nothing -> Filled piece {_hasMoved = True}
          Just pieceType -> Filled piece {_pieceType = pieceType, _hasMoved = True}
      update = [(move._start, Free), (move._end, newSquare)]
      final = case move._capture of
        (Just EnPassantCapture) -> (enPassantSquare move._start move._end, Free) : update
        _ -> update
      newBoard = board // final
      counter = case (move, oldSquare) of
        (NormalMove {_capture = Just _}, _) -> curState._drawMoveCounter + 1
        (_, Filled Piece {_pieceType = Pawn}) -> curState._drawMoveCounter + 1
        _ -> 0
   in GameState {_toPlay = other curState._toPlay, _board = Board newBoard, _drawMoveCounter = counter, _prevBoard = Just curState._board}

applyCastleMove :: GameState -> CastleMove -> GameState
applyCastleMove curState castleMove =
  let ((kStart, kEnd), (rStart, rEnd)) = case (curState._toPlay, castleMove) of
        (White, Kingside) -> (((1, 5), (1, 7)), ((1, 8), (1, 6)))
        (White, Queenside) -> (((1, 5), (1, 3)), ((1, 1), (1, 4)))
        (Black, Kingside) -> (((8, 5), (8, 7)), ((8, 8), (8, 6)))
        (Black, Queenside) -> (((8, 5), (8, 3)), ((8, 1), (8, 4)))
      king = getPieceAtSquare curState._board kStart
      rook = getPieceAtSquare curState._board rStart
      (Board boardArr) = curState._board
      newBoard = case (king, rook) of
        (Just king'@Piece {_pieceType = King, _hasMoved = False}, Just rook'@Piece {_pieceType = Rook, _hasMoved = False}) ->
          let newKingSquare = Filled $ king' {_hasMoved = True}
              newRookSquare = Filled $ rook' {_hasMoved = True}
           in boardArr // [(kStart, Free), (kEnd, newKingSquare), (rStart, Free), (rEnd, newRookSquare)]
        _ -> error $ "invalid castle move: " ++ show (king, rook)
   in GameState {_toPlay = other curState._toPlay, _board = Board newBoard, _drawMoveCounter = curState._drawMoveCounter + 1, _prevBoard = Just curState._board}

getMovesForPiece :: Bool -> GameState -> (PieceType, BoardIndex) -> [Move]
getMovesForPiece castle gameState (pType, boardIndex) = case pType of
  Pawn -> getPawnMoves gameState._prevBoard gameState boardIndex
  Bishop -> getBishopMoves gameState._board gameState._toPlay boardIndex
  Rook -> getRookMoves gameState._board gameState._toPlay boardIndex
  Queen -> getQueenMoves gameState._board gameState._toPlay boardIndex
  Knight -> getKnightMoves gameState._board gameState._toPlay boardIndex
  King -> getKingMoves castle gameState._board gameState._toPlay boardIndex

getPawnMoves :: Maybe Board -> GameState -> BoardIndex -> [Move]
getPawnMoves prevBoard curState (rank, file) =
  getForwardPawnMoves curState (rank, file)
    ++ getPawnCaptures curState (rank, file)
    ++ case prevBoard of
      Nothing -> []
      (Just prevBoard') -> getEnPassant prevBoard' curState (rank, file)

getForwardPawnMoves :: GameState -> BoardIndex -> [Move]
getForwardPawnMoves curState (rank, file) = getSingleSquareMove curState (rank, file) ++ getDoubleSquareMove curState (rank, file)

getSingleSquareMove :: GameState -> BoardIndex -> [Move]
getSingleSquareMove state ix@(rank, file) =
  let target =
        if state._toPlay == White
          then (rank + 1, file)
          else (rank - 1, file)
      canMoveTo = isValidIndex state._board target && isFree state._board target
      promote = state._toPlay == White && rank == 7 || state._toPlay == Black && rank == 2
   in if canMoveTo
        then
          if promote
            then getPromotionMoves ix Nothing target
            else [mkNonCaptureMove ix target]
        else []

getDoubleSquareMove :: GameState -> BoardIndex -> [Move]
getDoubleSquareMove state ix@(rank, file) =
  let target = if state._toPlay == White then (4, file) else (5, file)
      canMoveTo = (state._toPlay == White && rank == 2 || state._toPlay == Black && rank == 7) && isFree state._board target
   in ([Normal NormalMove {_start = ix, _end = target, _capture = Nothing, _promote = Nothing} | canMoveTo])

getPawnCaptures :: GameState -> BoardIndex -> [Move]
getPawnCaptures state ix@(rank, file) =
  let targets = if state._toPlay == White then [(rank + 1, file + 1), (rank + 1, file - 1)] else [(rank - 1, file + 1), (rank - 1, file + 1)]
      valid = filter (isValidIndex state._board) targets
      otherSide = if state._toPlay == Black then White else Black
      enemyPieces = Set.fromList [ix' | (ix', _) <- filter (uncurry $ (flip $ const isSide) otherSide) (getPieceLocations state._board)]
      matches = filter (`Set.member` enemyPieces) valid
      promote = state._toPlay == White && rank == 7 || state._toPlay == Black && rank == 2
   in if promote
        then matches >>= getPromotionMoves ix (Just NormalCapture)
        else [mkCaptureMove ix ix' | ix' <- matches]

getPromotionMoves :: BoardIndex -> Maybe Capture -> BoardIndex -> [Move]
getPromotionMoves startIx capture endIx =
  let move = NormalMove {_start = startIx, _end = endIx, _capture = capture, _promote = Nothing}
      pieces = [Queen, Knight, Bishop, Rook]
   in [Normal $ move {_promote = Just piece} | piece <- pieces]

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
      mkMove (rank', file') = Normal NormalMove {_start = (rank, file), _end = (rank', file'), _capture = Just EnPassantCapture, _promote = Nothing}
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
      directions = [map (,startFile) fwd, map (,startFile) bwd, map (startRank,) right, map (startRank,) left]
   in getDirectionMoves board side startIx directions

getQueenMoves :: Board -> Side -> BoardIndex -> [Move]
getQueenMoves board side startIx = getRookMoves board side startIx ++ getBishopMoves board side startIx

getKnightMoves :: Board -> Side -> BoardIndex -> [Move]
getKnightMoves board side startIx@(startRank, startFile) =
  let potentialDestinations =
        [ (startRank - 2, startFile - 1),
          (startRank - 2, startFile + 1),
          (startRank + 2, startFile - 1),
          (startRank + 2, startFile + 1),
          (startRank - 1, startFile - 2),
          (startRank - 1, startFile + 2),
          (startRank + 1, startFile - 2),
          (startRank + 1, startFile + 2)
        ]
      validDestinations = [x | x <- potentialDestinations, isValidIndex board x, not $ hasPieceColor board side x]
      (free, capture) = partition (isFree board) validDestinations
   in map (mkNonCaptureMove startIx) free ++ map (mkCaptureMove startIx) capture

getKingMoves :: Bool -> Board -> Side -> BoardIndex -> [Move]
getKingMoves castle board side startIx =
  let castleMoves = if castle then getCastleMoves board side startIx else []
   in castleMoves ++ getNormalKingMoves board side startIx

getCastleMoves :: Board -> Side -> BoardIndex -> [Move]
getCastleMoves board side startIx = case getPieceAtSquare board startIx of
  (Just Piece {_pieceType = King, _hasMoved = hasMoved, _player = color})
    | color /= side -> error "called getCastleMoves on square with opposite side king"
    | hasMoved || not canCastle -> []
    | otherwise -> getKingsideCastle board side startIx ++ getQueensideCastle board side startIx
  _ -> error "called getCastleMoves on square without king"
  where
    canCastle = (side == White && startIx == (1, 5)) || (side == Black && startIx == (8, 5))

getKingsideCastle :: Board -> Side -> BoardIndex -> [Move]
getKingsideCastle board side startIx@(startRank, startFile) =
  let squaresToCheck = map (startRank,) [startFile + 1, startFile + 2]
      free = [x | x <- squaresToCheck, isValidIndex board x, isFree board x, (not . squareIsInCheck side board startIx) x]
      allFree = length free == 2
      rook = getPieceAtSquare board (startRank, 8)
      rookCanCastle = case rook of
        (Just Piece {_pieceType = Rook, _hasMoved = False}) -> True
        _ -> False
   in [Castle Kingside | (not . squareIsInCheck side board startIx) startIx && allFree && rookCanCastle]

getQueensideCastle :: Board -> Side -> BoardIndex -> [Move]
getQueensideCastle board side startIx@(startRank, startFile) =
  let squaresToCheck = map (startRank,) [startFile - 1, startFile - 2]
      free = [x | x <- squaresToCheck, isValidIndex board x, isFree board x, (not . squareIsInCheck side board startIx) x]
      allFree = length free == 2
      rook = getPieceAtSquare board (startRank, 1)
      rookCanCastle = case rook of
        (Just Piece {_pieceType = Rook, _hasMoved = False}) -> True
        _ -> False
      knightSquare = (startRank, 2)
   in [Castle Queenside | (not . squareIsInCheck side board startIx) startIx && allFree && rookCanCastle && isFree board knightSquare]

squareIsInCheck :: Side -> Board -> BoardIndex -> BoardIndex -> Bool
squareIsInCheck side (Board board) startIx ix =
  let testBoard = Board $ board // [(startIx, Free), (ix, board ! startIx)]
      attackingPieces = filter (\(_, piece) -> piece._player == other side) (getPieceLocations testBoard) <&> \(x, y) -> (y._pieceType, x)
      testGameState = GameState {_board = testBoard, _toPlay = other side, _drawMoveCounter = 0, _prevBoard = Just (Board board)}
      moves = [endIx' | (Normal NormalMove {_end = endIx', _capture = Just NormalCapture}) <- attackingPieces >>= getMovesForPiece False testGameState]
   in elem ix moves

kingIsInCheck :: Side -> Board -> Bool
kingIsInCheck side board =
  let kingIndex = [x | (x, Piece {_pieceType = King, _player = side'}) <- getPieceLocations board, side == side']
   in case kingIndex of
        [x] -> squareIsInCheck side board x x
        _ -> error $ printBoard board ++ "\ninvalid board, king is not present or there are multiple of " ++ show side

getNormalKingMoves :: Board -> Side -> BoardIndex -> [Move]
getNormalKingMoves board side startIx@(startRank, startFile) =
  let potentialDestinations =
        [ (startRank - 1, startFile - 1),
          (startRank - 1, startFile + 0),
          (startRank - 1, startFile + 1),
          (startRank + 0, startFile - 1),
          (startRank + 0, startFile + 1),
          (startRank + 1, startFile - 1),
          (startRank + 1, startFile + 0),
          (startRank + 1, startFile + 1)
        ]
      validDestinations = [x | x <- potentialDestinations, isValidIndex board x, not $ hasPieceColor board side x]
      (free, capture) = partition (isFree board) validDestinations
   in map (mkNonCaptureMove startIx) free ++ map (mkCaptureMove startIx) capture

mkCaptureMove :: BoardIndex -> BoardIndex -> Move
mkCaptureMove startIx endIx = Normal $ NormalMove {_start = startIx, _end = endIx, _capture = Just NormalCapture, _promote = Nothing}

mkNonCaptureMove :: BoardIndex -> BoardIndex -> Move
mkNonCaptureMove startIx endIx = Normal $ NormalMove {_start = startIx, _end = endIx, _capture = Nothing, _promote = Nothing}

getDirectionMoves :: Board -> Side -> BoardIndex -> [[BoardIndex]] -> [Move]
getDirectionMoves board side startIx directions =
  let validDirections = map (takeWhile (isValidIndex board)) directions
      (free, taken) = unzip $ map (span (isFree board)) validDirections
      captures = [x | (Just x) <- map safeHead taken, hasPieceColor board (other side) x]
   in map (mkNonCaptureMove startIx) (join free) ++ map (mkCaptureMove startIx) captures

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
