module Game where


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


randomPair : Seed -> Int -> Int -> ( ( Int, Int ), Seed )
randomPair seed min max = 
  Random.generate( Random.pair ( Random.int min max ) ( Random.int min max ) ) seed


placeMine : Int -> Int -> Square.Model -> Square.Model
placeMine row col square =
  if square.row == row && square.col == col
    then { square | isMined = True }
    else square


-- recursively place the mines randomly on the board
placeMines : Board.Model -> Int -> Seed -> Board.Model
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


initialState : Model
initialState = 
  let 
    blankBoard = List.concatMap (\n -> List.map(\m -> Square.init n m False False ) [0..9]) [0..9]
    seed = Random.initialSeed 33212
    board = placeMines blankBoard 10 seed
  in 
    UnfinishedGame(board)


-- update

type Action = Move Board.Action | Reset | NoOp Board.Action


update : Action -> Model -> Model
update action model =
  case action of
    Move act ->
      let newBoard = Board.update act (board model)
      in
        if Board.isLoss newBoard then FinishedGame newBoard Loss
        else if Board.isVictory newBoard then FinishedGame newBoard Victory
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
