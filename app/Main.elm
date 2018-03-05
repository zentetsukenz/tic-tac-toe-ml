import Html exposing (Html, div, text, table, tr, td, input, button)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, get, set, repeat, fromList, toList, indexedMap, filter, map, slice)
import List exposing (member)
import String exposing (toInt)

main =
    Html.program { init = initModel
                 , update = update
                 , subscriptions = subscriptions
                 , view = view
                 }


-- Model

type alias Model =
    { gameState : GameState
    , playerTurn : Int
    , winner : Int
    , allowedMoves : AllowedMoves
    , currentMove : Maybe Int
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

initModel : (Model, Cmd Msg)
initModel =
    let
        gameState = initGameState
        allowedMoves = getAllowedMoves gameState
        model = Model gameState 1 0 allowedMoves Nothing
    in
        (model, Cmd.none)

-- Update

type Msg = MakeMove
         | ChangeMove String
         | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MakeMove ->
            case model.currentMove of
                Just move ->
                    (updateModelWithMove move model, Cmd.none)

                Nothing ->
                    (model, Cmd.none)

        ChangeMove newMoveStr ->
            case toInt newMoveStr of
                Ok newMoveInt ->
                    ({ model | currentMove = Just newMoveInt }, Cmd.none)

                Err _ ->
                    (model, Cmd.none)

        Reset ->
            initModel

updateModelWithMove : Int -> Model -> Model
updateModelWithMove move model =
    if member move (toList model.allowedMoves) then
        let
            newGameState = set move model.playerTurn model.gameState
            newPlayerTurn = model.playerTurn * -1
            newWinner = getWinner newGameState
            newAllowedMoves = getAllowedMoves newGameState
        in
            { model | gameState = newGameState
                    , playerTurn = newPlayerTurn
                    , winner = newWinner
                    , allowedMoves = newAllowedMoves
                    , currentMove = Just move
            }
    else
        model


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

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- View

view : Model -> Html Msg
view model =
    let
        rowModelList =
            [ (0, model)
            , (1, model)
            , (2, model)
            ]
        rowModelArr = fromList rowModelList
        inputCurrentMove = case model.currentMove of
                               Just currentMove -> toString currentMove
                               Nothing -> ""
    in
        div []
            [ table [] (toList (map buildRow rowModelArr))
            , input [ onInput ChangeMove
                    , value inputCurrentMove
                    , type_ "number"
                    ] []
            , button [ onClick MakeMove ] [ text "Move!" ]
            , button [ onClick Reset ] [ text "Reset!" ]
            ]

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
