module Square where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- model

type alias Model = 
  { isMined: Bool
  , isRevealed: Bool
  , adjacentMineCount: Int
  }


init : Bool -> Bool -> Int -> Model
init isMined isRevealed adjacentMineCount =
  { isMined = isMined
  , isRevealed = isRevealed
  , adjacentMineCount = adjacentMineCount
  }


isMineRevealed : Model -> Bool
isMineRevealed square = square.isMined && square.isRevealed


-- update

type Action = Reveal

update : Action -> Model -> Model
update action model =
  case action of 
    Reveal -> { model | isRevealed = True }


-- view

view : Signal.Address Action -> Model -> Html
view address model = 
  if (model.isRevealed)
    then
      td
        [ style 
          [ ("border", "1px solid black")
          , ("background-color", if model.isMined then "red" else "gray")
          , ("text-align", "center")
          , ("height", "48px")
          , ("width", "48px")
          , ("font-size", "10px")
          ]
        ]
        [text (if model.adjacentMineCount > 0 && (not model.isMined) then toString model.adjacentMineCount else "")]
    else
      td 
        [ onClick address Reveal
        , style
          [ ("border", "1px solid black")
          , ("background-color", if model.isMined then "red" else "blue")
          , ("height", "48px")
          , ("width", "48px")
          ]
        ] 
        []