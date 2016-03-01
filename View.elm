module View where

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Update exposing (..)


drawBoard : Signal.Address Action -> Board -> Html
drawBoard address board =
  let multiDimensionalBoard = Model.toMultiDimensional board
  in
    div [] 
      [ table 
        [ style 
          [ ("width", "100%")
          , ("height", "500px")
          , ("table-layout", "fixed")
          ]
        ]
        [ tbody []
          (toList (Array.map (drawRow address board) (Model.toMultiDimensional board)))
        ]
      ]


drawRow : Signal.Address Action -> Board -> Array Square -> Html
drawRow address board row = tr [] (toList (Array.map (drawCell address board) row))


drawCell : Signal.Address Action -> Board -> Square -> Html
drawCell address board square = 
  if (square.isRevealed)
    then
      let neighbors = Model.getNeighbors square board
          mineCount = List.length (List.filter (.isMined) neighbors)
      in
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
        [ onClick address (Click(square.row, square.col))
        , style
          [ ("border", "1px solid black")
          , ("background-color", "blue")
          , ("height", "48px")
          , ("width", "48px")
          ]
        ] 
        []



view : Signal.Address Action -> GameState -> Html
view address state =
  div [ style [ ("margin", "100px auto"), ("width", "500px") ] ] [
    case state of
      FinishedGame board result -> 
        div [ style [ ("text-align", "center"), ("font-size", "36px")] ] 
          [ drawBoard address board
          , text (toString result)
          , div []
            [ button [ onClick address Reset ] [ text "New Game" ] ]
          ]
      UnfinishedGame board -> drawBoard address board
  ]
