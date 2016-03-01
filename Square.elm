module Square where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- model

type alias Model = 
  { row: Int
  , col: Int
  , isMined: Bool
  , isRevealed: Bool
  }


init : Int -> Int -> Bool -> Bool -> Model
init row col isMined isRevealed =
  { row = row
  , col = col
  , isMined = isMined
  , isRevealed = isRevealed
  }


isMineRevealed : Model -> Bool
isMineRevealed square = square.isMined && square.isRevealed

isNeighbor: Model -> Model -> Bool
isNeighbor square1 square2 = 
  abs ( square1.row - square2.row ) <= 1 && 
  abs ( square1.col - square2.col ) <= 1


-- update

type Action = Reveal

update : Action -> Model -> Model
update action model =
  case action of 
    Reveal -> { model | isRevealed = True }


-- view

view : Signal.Address Action -> Model -> Int -> Html
view address square mineCount = 
  if (square.isRevealed)
    then
      td
        [ style 
          [ ("border", "1px solid black")
          , ("background-color", if square.isMined then "red" else "gray")
          , ("text-align", "center")
          , ("height", "48px")
          , ("width", "48px")
          , ("font-size", "10px")
          ]
        ]
        [text (if mineCount > 0 && (not square.isMined) then toString mineCount else "")]
    else
      td 
        [ onClick address Reveal
        , style
          [ ("border", "1px solid black")
          , ("background-color", "blue")
          , ("height", "48px")
          , ("width", "48px")
          ]
        ] 
        []