module Update where

import Model exposing (processMove)


type alias ID = (Int, Int)


type Action = Click ID | Reset

update : Action -> Model.GameState -> Model.GameState
update action model =
  case action of
    Click id -> Model.processMove (fst id) (snd id) model
    Reset -> Model.initialState
