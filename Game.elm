module Game where


import Array exposing (..)
import Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import Square exposing (..)


-- model

type Result = Victory | Loss


type Model
  = FinishedGame Board.Model Result
  | UnfinishedGame Board.Model


board : Model -> Board.Model
board model =
  case model of
    FinishedGame board _ -> board
    UnfinishedGame board -> board


isLoss : Board.Model -> Bool
isLoss board = Board.any Square.isMineRevealed board
  

isVictory : Board.Model -> Bool
isVictory board =
  not (Board.any (\square -> (not square.isMined) && (not square.isRevealed)) board)


randomPair : Seed -> Int -> Int -> ( ( Int, Int ), Seed )
randomPair seed min max = 
  Random.generate( Random.pair ( Random.int min max ) ( Random.int min max ) ) seed



-- recursively place the mines randomly on the board
placeMines : Board.Model -> Int -> Seed -> Board.Model
placeMines board numMines seed =
  if numMines == 0
    then board
    else
      let rnd = randomPair seed 0 ( Array.length board )
          coords = fst rnd
          newSeed = snd rnd
          row = fst coords
          col = snd coords
          maybeSquare = Board.find row col board
      in
        case maybeSquare of
          Just square -> 
            if square.isMined
              then placeMines board numMines newSeed
              else
                let boardRow = Maybe.withDefault Array.empty ( Array.get row board )
                    updatedRow = Array.set col ( Square.init True False 0 ) boardRow
                    newBoard = Array.set row updatedRow board
                in
                  placeMines newBoard ( numMines - 1 ) newSeed
          Nothing -> placeMines board numMines newSeed


setMineCounts : Board.Model -> Board.Model
setMineCounts board =
  let getCount rowIndex colIndex = Board.getNeighbors rowIndex colIndex board |> List.filter ( .isMined ) |> List.length
      setCount rowIndex colIndex square = { square | adjacentMineCount = getCount rowIndex colIndex }
  in
    Array.indexedMap ( \rowIndex row -> Array.indexedMap ( setCount rowIndex ) row ) board


initialState : Model
initialState = 
  let 
    blankBoard = Board.init 8
    seed = Random.initialSeed 33212
    board = placeMines blankBoard 10 seed
    boardWithMineCounts = setMineCounts board
  in 
    UnfinishedGame(boardWithMineCounts)


-- update

type Action = Move Board.Action | Reset | NoOp Board.Action


update : Action -> Model -> Model
update action model =
  case action of
    Move act ->
      let newBoard = Board.update act (board model)
      in
        if isLoss newBoard then FinishedGame newBoard Loss
        else if isVictory newBoard then FinishedGame newBoard Victory
        else UnfinishedGame newBoard
    Reset -> initialState
    NoOp _ -> model



-- view

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ ("margin", "100px auto"), ("width", "500px") ] ] [
    case model of
      FinishedGame board result -> 
        div [ style [ ("text-align", "center"), ("font-size", "36px")] ] 
          [ Board.view (Signal.forwardTo address NoOp) board
          , text (toString result)
          , div []
            [ button [ onClick address Reset ] [ text "New Game" ] ]
          ]
      UnfinishedGame board -> Board.view (Signal.forwardTo address Move) board
  ]
