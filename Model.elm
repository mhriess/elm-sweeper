module Model where

import Array exposing (..)
import Random exposing (..)


type Result = Victory | Loss


type alias Square = 
  { row: Int
  , col: Int
  , isMined: Bool
  , isRevealed: Bool
  }


type alias Board = List Square


type GameState
  = FinishedGame Board Result
  | UnfinishedGame Board


-- extract board from game state
board : GameState -> Board
board state =
  case state of
    FinishedGame board _ -> board
    UnfinishedGame board -> board


randomPair : Seed -> Int -> Int -> ( ( Int, Int ), Seed )
randomPair seed min max = 
  Random.generate( Random.pair ( Random.int min max ) ( Random.int min max ) ) seed


placeMine : Int -> Int -> Square -> Square
placeMine row col square =
  if square.row == row && square.col == col
    then { square | isMined = True }
    else square


-- recursively place the mines randomly on the board
placeMines : Board -> Int -> Seed -> Board
placeMines board numMines seed =
  if numMines == 0
    then board
    else
      let rnd = randomPair seed 0 ( List.length board )
          coords = fst rnd
          newSeed = snd rnd
          row = fst coords
          col = snd coords
          maybeSquare = List.head ( List.filter ( \sq -> sq.row == row && sq.col == col ) board )
      in
        case maybeSquare of
          Just square -> 
            if square.isMined
              then placeMines board numMines newSeed
              else
                let newBoard =
                  List.map ( placeMine row col ) board
                in
                  placeMines newBoard ( numMines - 1 ) newSeed
          Nothing -> placeMines board numMines newSeed


initializeSquare : Int -> Int -> Square
initializeSquare row col = { row = row, col = col, isMined = False, isRevealed = False }


initialState : GameState
initialState = 
  let 
    blankBoard = List.concatMap (\n -> List.map(\m -> initializeSquare n m) [0..9]) [0..9]
    seed = Random.initialSeed 33212
    board = placeMines blankBoard 10 seed
  in 
    UnfinishedGame(board)
  

isLoss : Board -> Bool
isLoss board = List.any isMineRevealed board


isVictory : Board -> Bool
isVictory board =
  not (List.any (\square -> (not square.isMined) && (not square.isRevealed)) board)


isMineRevealed : Square -> Bool
isMineRevealed square = square.isMined && square.isRevealed


processMove : Int -> Int -> GameState -> GameState
processMove row col state =
  case state of
    FinishedGame _ _ -> state
    _ ->
      let newBoard = List.map (updateSquare row col) (board state)
      in
        if isLoss newBoard then FinishedGame newBoard Loss
        else if isVictory newBoard then FinishedGame newBoard Victory
        else UnfinishedGame (openNeighbors row col newBoard)


updateSquare : Int -> Int -> Square -> Square
updateSquare row col square =
  if square.row == row && square.col == col
      then { square | isRevealed = True }
      else square


toMultiDimensional : Board -> Array (Array Square)
toMultiDimensional board =
  let
    boardLength = board |> List.length |> toFloat |> sqrt |> round
    initial = Array.initialize boardLength (\n -> Array.initialize boardLength (\m -> Square 0 0 False False))
  in 
    foldl toMultiDimensional' initial (Array.fromList board)


toMultiDimensional' : Square -> Array (Array Square) -> Array (Array Square)
toMultiDimensional' square acc =
  let row = Maybe.withDefault Array.empty (get square.row acc)
      col = set square.col square row
      updated = set square.row col acc
  in
    updated


getNeighbors : Square -> Board -> List Square
getNeighbors square board =
  List.filter (isNeighbor square.row square.col) board


isNeighbor: Int -> Int -> Square -> Bool
isNeighbor r c square = 
  abs ( square.row - r ) <= 1 && abs ( square.col - c ) <= 1


openNeighbors : Int -> Int -> Board -> Board
openNeighbors row col board =
  let shouldReveal square r c = ( isNeighbor r c square ) && ( not square.isMined )
  in
    List.map (\square -> if (shouldReveal square row col) then { square | isRevealed = True } else square ) board


