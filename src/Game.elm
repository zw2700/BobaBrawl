module Game exposing (..)

{-| This file handles all the game logic and provides the Gameplay interface to the Main application.alias.

The core parts you need to implement are:

1.  A type for your Game model
2.  An initialisation function that takes a Settings record and returns a Game record
3.  A Msg type that represents all the possible messages that can be sent from the interface to the game logic
4.  An update function that takes a Msg and a Game and returns a new Game
5.  A view function that takes a Game and returns Html Msg (the interface for the game)

You'll probably want to implement a lot of helper functions to make the above easier.

-}

import Array exposing (..)
import Common exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Process
import Settings exposing (..)
import Task
import Tuple exposing (..)



--------------------------------------------------------------------------------
-- GAME MODEL
--------------------------------------------------------------------------------


{-| A record type which contains all of the game state.

This needs to be sufficiently detailed to represent the entire game state, i.e.
if you save this record, turn off your computer, and then reload this record,
you should be able to pick up the game exactly where you left off.

We also need some metadata including the settings used to initialise
the game, the status (whether it's still going or completed), and
whose turn it currently is.

You might also like to pre-calculate some data and store it here
if you will use it a lot.

-}
type alias Game =
    { settings : Settings
    , count : Int
    , status : Status
    , turn : Player
    , countPerRow: List Int
    , cup : Dict Coord Cell
    }


{-| A cell, can be either empty or filled with boba
-}
type Cell
    = Empty
    | Filled

{-| One row in cup
-}
type alias Row = Dict Int Cell


{-| Create the initial game data given the settings.
-}
init : Settings -> ( Game, Cmd Msg )
init settings =
    let
        countPerRow = init_countPerRow settings

        initialGame =
            { settings = settings
            , count = settings.initialCount
            , status = Playing
            , turn = Player1
            , countPerRow = countPerRow
            , cup = init_cup settings countPerRow
            }
    in
    ( initialGame, Cmd.none )



--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = Increment
    | Decrement
    | Sip Coord


{-| Apply a move to a game state, returning a new game state.
-}
applyMove : Move -> Game -> Game
applyMove move game =
    case move of
        Increment ->
            { game | count = game.count + 1 }

        Decrement ->
            { game | count = game.count - 1 }

        Sip (x, y) ->
            let newGame = sipAtLocation (x, y) game
            in
            if newGame.settings.bubbleCount == 0 then
                { newGame | status = Complete (Winner newGame.turn) }
            else
                { newGame | turn = opponent game.turn }


--------------------------------------------------------------------------------
-- INTERFACE LOGIC
--
-- This section deals with how to map the interface to the game logic.
--
-- Msg contains messages that can be sent from the game interface. You should then
-- choose how to handle them in terms of game logic.
--
-- This also sets scaffolding for the computer players - when a computer player
-- makes a move, they generate a message (ReceivedComputerMove) which is then handled
-- just like a player interacting with the interface.
--------------------------------------------------------------------------------


{-| An enumeration of all messages that can be sent from the interface to the game
-}
type Msg
    = ClickedIncrement
    | ClickedDecrement
    | ClickedCell Coord
    | PauseThenMakeComputerMove
    | ReceivedComputerMove Move
    | NoOp


{-| A convenience function to pipe a command into a (Game, Cmd Msg) tuple.
-}
withCmd : Cmd Msg -> Game -> ( Game, Cmd Msg )
withCmd cmd game =
    ( game, cmd )


{-| The main update function for the game, which takes an interface message and returns
a new game state as well as any additional commands to be run.
-}
update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        ClickedIncrement ->
            game
                |> applyMove Increment
                |> withCmd Cmd.none

        ClickedDecrement ->
            game
                |> applyMove Decrement
                |> withCmd Cmd.none

        ClickedCell (x, y) ->
            let nextState = applyMove (Sip (x, y)) game
            in
            case nextState.status of
                Playing ->
                    case game.settings.playMode of
                        PlayHumanVsHuman ->
                            nextState
                                |> withCmd Cmd.none

                        -- If the game is continuing and it's the computer's turn, then we need to generate a move.
                        -- To make it more "human-like", pause for 250 milliseconds before generating a move.
                        _ ->
                            nextState
                                |> withCmd (Task.perform (\_ -> PauseThenMakeComputerMove) (Process.sleep 250))

                Complete _ ->
                    nextState
                        |> withCmd Cmd.none

        PauseThenMakeComputerMove ->
            case game.settings.computerDifficulty of
                Settings.Easy ->
                    game |> withCmd (makeComputerMoveEasy game)

                Settings.Hard ->
                    game |> withCmd (makeComputerMoveHard game)

        ReceivedComputerMove move ->
            applyMove move game
                |> withCmd Cmd.none

        NoOp ->
            game
                |> withCmd Cmd.none


--------------------------------------------------------------------------------
-- COMPUTER: EASY PLAYER
--------------------------------------------------------------------------------

{-| Logic for an "easy" computer player.

This is a simple player which sorts the possible moves by
a basic score (based on the number of neighbours next to the move)
and then randomly picks one out of the top 5.

Randomness is a little more tricky in Elm than other languages as Elm is pure,
so this might be useful as a reference of how you might use Randomness in your
computer AI players. Essentially any random events get wrapped in a
Random type that you need to map/andThen under (ala Haskell monads).

Your computer player function takes a game and returns a move, which you
then wrap in ReceivedComputerMove to create a Cmd Msg.

-}
makeComputerMoveEasy : Game -> Cmd Msg
makeComputerMoveEasy game = Task.perform ReceivedComputerMove (Task.succeed (Sip (0, 0))) 


{-| Very similar to the easy player.

This function is deterministic however, so the way it is returned
is slightly different (wrap in Task.perform).

This player is slightly better than the Easy player, and will score
cells based on the size of the largest row of stones it will make
for you or your opponent. It's not that challenging to beat, however.

-}
makeComputerMoveHard : Game -> Cmd Msg
makeComputerMoveHard game = Task.perform ReceivedComputerMove (Task.succeed (Sip (0, 0))) 


--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------


{-| The main view function that gets called from the Main application.

Essentially, takes a game and projects it into a HTML interface where Messages
can be sent from.

-}
-- view : Game -> Html Msg
-- view game =
--     div [ id "game-screen-container" ]
--         [ h1 [id "counter-value"] [ text (String.fromInt game.count) ]
--         , div [id "counter-buttons"]
--             [ button [ onClick ClickedDecrement ] [ text "-" ]
--             , button [ onClick ClickedIncrement ] [ text "+" ]
--             ]
--         ]
view : Game -> Html Msg
view game =
    div [ id "game-screen-container" ]
        [ div [ id "game-header" ] [ viewStatus game ]
        , div [ id "game-main" ] [ viewCup game ]
        ]

{-| View game status at the top of the game board
-}
viewStatus : Game -> Html Msg
viewStatus ({ settings } as game) =
    let
        colour =
            case game.status of
                Complete (Winner Player1) ->
                    settings.player1Colour |> Settings.colourToString

                Complete (Winner Player2) ->
                    settings.player2Colour |> Settings.colourToString

                Playing ->
                    currentColour game |> Settings.colourToString

        ( statusClass, statusText ) =
            case game.status of
                Playing ->
                    case settings.playMode of
                        PlayHumanVsHuman ->
                            ( "status-playing", currentName game ++ "'s turn." )

                        PlayComputerVsMe ->
                            case game.turn of
                                Player1 ->
                                    ( "status-thinking", currentName game ++ " is thinking..." )

                                Player2 ->
                                    ( "status-playing", "Your turn." )

                        PlayMeVsComputer ->
                            case game.turn of
                                Player1 ->
                                    ( "status-playing", "Your turn." )

                                Player2 ->
                                    ( "status-thinking", currentName game ++ " is thinking..." )

                Complete (Winner Player1) ->
                    case settings.playMode of
                        PlayHumanVsHuman ->
                            ( "status-won", currentName game ++ " WINS!" )

                        PlayComputerVsMe ->
                            ( "status-lost", "You lost..." )

                        PlayMeVsComputer ->
                            ( "status-won", "You win!" )

                Complete (Winner Player2) ->
                    case settings.playMode of
                        PlayHumanVsHuman ->
                            ( "status-won", currentName game ++ " WINS!" )

                        PlayComputerVsMe ->
                            ( "status-won", "You win!" )

                        PlayMeVsComputer ->
                            ( "status-lost", "You lost...)" )
    in
    div [ id "game-status", class statusClass, class colour ]
        [ div [ class ("game-status-text " ++ colour) ] [ text statusText ]
        , div [ class "firework-container", classList [ ( "show", statusClass == "status-won" ) ] ]
            [ div [ class "firework" ] []
            , div [ class "firework" ] []
            , div [ class "firework" ] []
            ]
        , div
            [ class "flash"
            , class statusClass
            , classList [ ( "show", statusClass == "status-won" || statusClass == "status-lost" || statusClass == "status-draw" ) ]
            ]
            []
        ]

{-| Actual game view
-}
viewCup : Game -> Html Msg
viewCup game =
    let
        countPerRow = game.countPerRow
        bottomToTopRatio = toFloat(getListValue countPerRow 0) / toFloat(getListValue countPerRow 9)

        cellView : (Coord, Cell) -> Html Msg
        cellView ((x, y), cell) =
            let
                onClickEvent =
                    if cell == Filled then
                        case game.status of
                            Playing ->
                                case game.settings.playMode of
                                    PlayHumanVsHuman ->
                                        ClickedCell (x, y)

                                    PlayComputerVsMe ->
                                        case game.turn of
                                            Player1 ->
                                                NoOp

                                            Player2 ->
                                                ClickedCell (x, y)

                                    PlayMeVsComputer ->
                                        case game.turn of
                                            Player1 ->
                                                ClickedCell (x, y)

                                            Player2 ->
                                                NoOp

                            Complete _ ->
                                NoOp

                    else
                        NoOp

                cellColour =
                    case cell of
                        Empty ->
                            "empty"

                        Filled ->
                            "filled"
            in
            div
                [ class "game-cell-container", style "grid-row" (String.fromInt x ++ " / span 1"), style "grid-column" (String.fromInt y ++ " / span 1") , onClick onClickEvent]
                [ div [ class "game-cell", class cellColour ]
                    [div [] [ text (String.fromInt x ++ "," ++ String.fromInt y) ]]
                ]
            -- div
            --     [ class "game-cell-container"
            --     , onClick onClickEvent
            --     ]
            --     [ div [ class "game-cell", class cellColour ]
            --         [div [ class "game-cell-marker", class cellColour ] []]
            --     ]
    in
    div
        [ id "cup-container"
        , style "clip-path" ("polygon(" ++ String.fromFloat((1 - bottomToTopRatio) * 50) ++ "% 100%," ++ String.fromFloat(100 - (1 - bottomToTopRatio) * 50) ++ "% 100%, 100% 0, 0 0)")
        , style "height" (String.fromFloat(800 / (toFloat( getListValue countPerRow 9 ))) ++ "vh")
        ]
        [ div
            [ id "cup"]
            (List.map cellView (Dict.toList game.cup))
        ]

--------------------------------------------------------------------------------
-- GAME HELPER FUNCTIONS
-- Helper functions to implement the game logic.
--------------------------------------------------------------------------------

{-| Initialize count per row
-}
init_countPerRow : Settings -> List Int
init_countPerRow settings =
    let
        findRowCount x = floor((toFloat x) * settings.cupSlope + settings.cupWidth)
    in
    List.map findRowCount (List.range 0 9)


{-| Create the initial game data given the settings.
-}
init_cup : Settings -> List Int -> Dict Coord Cell
init_cup settings countPerRow =
    let
        defaultRecord = {item = ((0, 0), Filled), 
                        row_start = 0, 
                        bubblesLeft = settings.bubbleCount}

        init_cup_helper {item, row_start, bubblesLeft} =
            let
                cell = if bubblesLeft > 1 then Filled else Empty
                new_row_start = if (Tuple.first (Tuple.first item)) < 9 then row_start + (getListValue countPerRow (Tuple.first (Tuple.first item))) - (getListValue countPerRow ((Tuple.first (Tuple.first item))+1))
                                else row_start
            in
            if (Tuple.second (Tuple.first item)) >= row_start + 2 * ((getListValue countPerRow (Tuple.first (Tuple.first item)))-1) then
                if (Tuple.first (Tuple.first item)) == 9 then 
                    Nothing
                else
                    Just{item = ((((Tuple.first (Tuple.first item)) + 1), new_row_start), cell), 
                            row_start = new_row_start, 
                            bubblesLeft = bubblesLeft - 1}
            else
                Just{item = (((Tuple.first (Tuple.first item)), ((Tuple.second (Tuple.first item)) + 2)), cell), 
                            row_start = row_start, 
                            bubblesLeft = bubblesLeft - 1}
        iteratedList = List.Extra.iterate init_cup_helper defaultRecord
        
    in
    List.map .item iteratedList
        |> Dict.fromList


{-| Helper function to sip at a location
-}
sipAtLocation: Coord -> Game -> Game
sipAtLocation (x, y) game =
    let
        cell = Maybe.withDefault Empty (Dict.get (x, y) game.cup)
    in
    case cell of
        Empty ->
            game

        Filled ->
            { game | cup = Dict.update (x, y) (\_ -> Just Empty) game.cup, settings = decrementBubbleCount game.settings}
                |> dropAtLocation (x, y)

{-| Helper function to fill bubbles at a location by dropping bubbles from above
-}
dropAtLocation : Coord -> Game -> Game
dropAtLocation (x, y) game =
    let
        cell = Maybe.withDefault Empty (Dict.get (x, y) game.cup)
    in
    case cell of
        Empty ->
            if (Dict.member (x + 1, y) game.cup) && (Maybe.withDefault Empty (Dict.get (x + 1, y) game.cup)) == Filled then
                dropAtLocation (x + 1, y) { game | cup = game.cup 
                                                    |> Dict.update (x, y) (\_ -> Just Filled) 
                                                    |> Dict.update (x + 1, y) (\_ -> Just Empty) }
            else if (Dict.member (x + 1, y + 1) game.cup) && (Maybe.withDefault Empty (Dict.get (x + 1, y + 1) game.cup)) == Filled then
                dropAtLocation (x + 1, y + 1) { game | cup = game.cup 
                                                    |> Dict.update (x, y) (\_ -> Just Filled) 
                                                    |> Dict.update (x + 1, y + 1) (\_ -> Just Empty) }
            else if (Dict.member (x + 1, y - 1) game.cup) && (Maybe.withDefault Empty (Dict.get (x + 1, y - 1) game.cup)) == Filled then
                dropAtLocation (x + 1, y - 1) { game | cup = game.cup 
                                                    |> Dict.update (x, y) (\_ -> Just Filled) 
                                                    |> Dict.update (x + 1, y - 1) (\_ -> Just Empty) }
            else
                game

        Filled ->
            game

{-| Helper function to decrement bubble count
-}
decrementBubbleCount : Settings -> Settings
decrementBubbleCount settings =
    {settings | bubbleCount = settings.bubbleCount - 1}


{-| Returns the colour of the current player
-}
currentColour : Game -> Settings.SimpleColour
currentColour game =
    case game.turn of
        Player1 ->
            game.settings.player1Colour

        Player2 ->
            game.settings.player2Colour


{-| Returns the name of the current player
-}
currentName : Game -> String
currentName game =
    case game.turn of
        Player1 ->
            game.settings.player1Name

        Player2 ->
            game.settings.player2Name