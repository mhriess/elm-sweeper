module Board where


import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Square


-- model


type alias Model = Array ( Array Square.Model )


init : Int -> Model
init size = Array.initialize size (\n -> Array.initialize size (\m -> Square.init False False 0 ) )


isNeighbor: Int -> Int -> Int -> Int -> Bool
isNeighbor row1 col1 row2 col2 = 
  abs ( row1 - row2 ) <= 1 && 
  abs ( col1 - col2 ) <= 1


getNeighbors : Int -> Int -> Model -> List Square.Model
getNeighbors row col model =
  let upperRowIndex = row - 1
      middleRowIndex = row
      bottomRowIndex = row + 1
      upperColIndex = col - 1
      middleColIndex = col
      bottomColIndex = col + 1

      upperRow = Maybe.withDefault Array.empty ( Array.get upperRowIndex model )
      middleRow = Maybe.withDefault Array.empty ( Array.get middleRowIndex model )
      bottomRow = Maybe.withDefault Array.empty ( Array.get bottomRowIndex model )
      
      ul = Array.get upperColIndex upperRow
      um = Array.get middleColIndex upperRow
      ur = Array.get bottomColIndex upperRow
      ml = Array.get upperColIndex middleRow
      mr = Array.get bottomColIndex middleRow
      bl = Array.get upperColIndex bottomRow
      bm = Array.get col bottomRow
      br = Array.get bottomColIndex bottomRow

      flatten maybeNeighbor neighbors =
        case maybeNeighbor of
          Just neighbor -> neighbor :: neighbors
          Nothing -> neighbors
  in
    List.foldl flatten [] (ul :: um :: ur :: ml :: mr :: bl :: bm :: br :: [])


-- any for array of arrays
any : ( Square.Model -> Bool ) -> Model -> Bool
any cond model = any' cond model False


any' : ( Square.Model -> Bool ) -> Model -> Bool -> Bool
any' cond model res =
  if ( res == True || Array.isEmpty model )
    then res
  else
    let head = Array.slice 0 1 model
        headFirst = Maybe.withDefault Array.empty ( Array.get 0 head )
        tail = Array.slice 1 ( Array.length model ) model
    in
      any' cond tail ( not ( Array.isEmpty ( Array.filter cond headFirst ) ) )


find : Int -> Int -> Model -> Maybe Square.Model
find rowI colI model =
  let row = Maybe.withDefault Array.empty ( Array.get rowI model )
  in
    Array.get colI row


-- update

type Action = Modify Int Int Square.Model Square.Action


update : Action -> Model -> Model
update action model = 
  case action of
    Modify rowX colX sq action ->
      let updateSquare rowY colY square =
            if ( rowX == rowY && colX == colY )
              then Square.update action sq
            else if ( isNeighbor rowX colX rowY colY ) && ( not square.isMined ) && ( not sq.isMined )
              then Square.update action square
            else
              square

      in
        Array.indexedMap ( \rowIndex row -> Array.indexedMap ( updateSquare rowIndex) row ) model

-- view

view : Signal.Address Action -> Model -> Html
view address model =
  table 
    [ style 
      [ ("width", "100%")
      , ("height", "500px")
      , ("table-layout", "fixed")
      ]
    ]
    [ tbody []
      ( toList ( Array.indexedMap ( viewRow address ) model ) )
    ]


viewRow : Signal.Address Action -> Int -> Array Square.Model -> Html
viewRow address rowIndex row =
  tr [] ( toList ( Array.indexedMap ( viewSquare address rowIndex ) row ) )


viewSquare : Signal.Address Action -> Int -> Int -> Square.Model -> Html
viewSquare address row col model =
  Square.view (Signal.forwardTo address (Modify row col model)) model
    

