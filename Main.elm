import StartApp.Simple exposing (start)

import Html exposing (..)
import Model exposing (..)
import Signal exposing (map, Signal, foldp, mergeMany, sampleOn)
import Update exposing (..)
import View exposing (..)


main =
  start
    { model = initialState
    , update = update
    , view = view
    }
