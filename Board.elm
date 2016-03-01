module Board where


import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Square


-- model


type alias Model = List Square.Model


getNeighbors : Square.Model -> Model -> List Square.Model
getNeighbors square board =
  List.filter (Square.isNeighbor square) board


isLoss : Model -> Bool
isLoss board = List.any Square.isMineRevealed board


isVictory : Model -> Bool
isVictory board =
  not (List.any (\square -> (not square.isMined) && (not square.isRevealed)) board)


toMultiDimensional : Model -> Array (Array Square.Model)
toMultiDimensional board =
  let
    boardLength = board |> List.length |> toFloat |> sqrt |> round
    initial = Array.initialize boardLength (\n -> Array.initialize boardLength (\m -> Square.init 0 0 False False))
  in 
    foldl toMultiDimensional' initial (Array.fromList board)


toMultiDimensional' : Square.Model -> Array (Array Square.Model) -> Array (Array Square.Model)
toMultiDimensional' square acc =
  let row = Maybe.withDefault Array.empty (get square.row acc)
      col = set square.col square row
      updated = set square.row col acc
  in
    updated


-- update

type Action = Reveal Square.Model Square.Action


update : Action -> Model -> Model
update action model = 
  case action of
    Reveal sq action ->
      let updateSquare square =
            if ( sq.row == square.row && sq.col == square.col )
              then Square.update action square
            else if ( Square.isNeighbor square sq ) && ( not square.isMined )
              then Square.update action square
            else
              square

      in
        List.map updateSquare model

-- view

view : Signal.Address Action -> Model -> Html
view address model =
  let multiDimensional = toMultiDimensional model
  in
    table 
      [ style 
        [ ("width", "100%")
        , ("height", "500px")
        , ("table-layout", "fixed")
        ]
      ]
      [ tbody []
        ( toList ( Array.map ( viewRow address model ) multiDimensional ) )
      ]

viewRow : Signal.Address Action -> Model -> Array Square.Model -> Html
viewRow address board row =
  tr []
    ( toList ( Array.map ( viewSquare address board ) row ) )

viewSquare : Signal.Address Action -> Model -> Square.Model -> Html
viewSquare address board model =
  let neighbors = getNeighbors model board
      mineCount = List.length (List.filter (.isMined) neighbors)
  in 
    Square.view (Signal.forwardTo address (Reveal model)) model mineCount

