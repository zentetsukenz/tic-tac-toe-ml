import Html exposing (Html, div, text, table, tr, td)
import Array exposing (Array, get, set, repeat, fromList, toList, indexedMap, filter, map, slice)
import List exposing (member)

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


-- Model

type alias Model =
    { gameState : GameState
    , playerTurn : Int
    , winner : Int
    , allowedMoves : AllowedMoves
    }

type alias GameState = Array Int
type alias AllowedMoves = Array Int

initGameState : GameState
initGameState =
    repeat 9 0

getAllowedMoves : GameState -> AllowedMoves
getAllowedMoves game_state =
    map extractIndex <| filter isNotMoved <| indexedMap (,) game_state

isNotMoved : (Int, Int) -> Bool
isNotMoved (_, state) =
    if state == 0 then
        True

    else
        False

extractIndex : (Int, Int) -> Int
extractIndex (index, _) =
    index

initModel : Model
initModel =
    let
        gameState = initGameState
        allowedMoves = getAllowedMoves gameState
    in
        { gameState = gameState
        , playerTurn = 1
        , winner = 0
        , allowedMoves = allowedMoves
        }

-- Update

type Msg = Move Int
         | Reset

update : Msg -> Model -> Model
update msg model =
    case msg of
        Move move ->
            if member move (toList model.allowedMoves) then
                let
                    newGameState = set move model.playerTurn model.gameState
                    newPlayerTurn = model.playerTurn * -1
                    newWinner = getWinner newGameState
                    newAllowedMoves = getAllowedMoves newGameState
                in
                    { gameState = newGameState
                    , playerTurn = newPlayerTurn
                    , winner = newWinner
                    , allowedMoves = newAllowedMoves
                    }
            else
                model

        Reset ->
            initModel

getWinner : GameState -> Int
getWinner gameState =
    let
        winConditions =
            [ [0, 1, 2]
            , [3, 4, 5]
            , [6 ,7, 8]
            , [0, 3, 6]
            , [1, 4, 7]
            , [2, 5 ,8]
            , [0, 4, 8]
            , [2, 4, 6]
            ]
    in
        gettingWinner gameState winConditions

gettingWinner : GameState -> List (List Int) -> Int
gettingWinner gameState winConditions =
    case winConditions of
        [] -> 0
        (winningCondition::winningConditions) ->
            let
                winningArray = fromList winningCondition

                a = getAt 0 winningArray
                b = getAt 1 winningArray
                c = getAt 2 winningArray

                aValue = getAt a gameState
                bValue = getAt b gameState
                cValue = getAt c gameState

                sumValue = aValue + bValue + cValue
            in
                case sumValue of
                    3 -> 1
                    (-3) -> -1
                    _ -> gettingWinner gameState winningConditions

getAt : Int -> Array Int -> Int
getAt index array =
    let
        result = get index array
    in
        case result of
            Just value ->
                value

            Nothing ->
                0

-- View

view : Model -> Html msg
view model =
    let
        rowModelList =
            [ (0, model)
            , (1, model)
            , (2, model)
            ]
        rowModelArr = fromList rowModelList
    in
        table [] (toList (map buildRow rowModelArr))

buildRow : (Int, Model) -> Html msg
buildRow (row, model) =
    let
        start = row * 3
        end = (row * 3) + 3

        rowValue = slice start end model.gameState
    in
        tr [] (toList (map buildCell rowValue))

buildCell : Int -> Html msg
buildCell player =
    let
        symbol = case player of
                     1 -> "O"
                     (-1) -> "X"
                     0 -> " "
                     _ -> "?"
    in
        td [] [ text symbol ]
