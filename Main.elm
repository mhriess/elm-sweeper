import StartApp.Simple exposing (start)

import Html exposing (..)
import Game exposing  (..)


main : Signal Html.Html
main =
  start
    { model = Game.initialState
    , update = Game.update
    , view = Game.view
    }
