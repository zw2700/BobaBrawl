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
import Set exposing (..)
import Svg exposing (Svg)
import Svg.Attributes 
import Svg.Events 
import Geometry.Svg as Svg
import Point2d 
import Polygon2d 
import Frame2d
import Circle2d
import Length
import Pixels
import Rectangle2d




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
    , status : Status
    , turn : Player
    -- Cup is a set of coordinates indicating where boba are
    -- Note: if it's not in the set, it's empty
    , cup : Set Coord
    , debug: List Coord
    }


{-| Initialise the set of boba. 
NOTE: the grid unit is equal to the boba's radius (i.e. half the diameter)
Therefore Boba are at least 2 units apart. 
This allows more natural packing of the Boba and preserves the drop behaviour
(i.e. boba dropping will need to check the cell 2 units below and 1 unit left, 0 units, and 1 unit right of it). 
Strategy:
1. Calculate the width of the row using trigonometry
2. Calculate how many boba can fit in this row 
3. Place the boba in this row 
4. Recurse to the next row until all boba have been used
-}
initCup : Settings -> Set Coord
initCup settings = 
    let
        -- Recursive function to implement the strategy above
        -- Maintain an accumulator of all the boba generated so far
        recursivelyFillRow rowNumber currentSet = 
            -- If we have generated the number of total boba allowed, then stop recursing and return
            if Set.size currentSet >= settings.bubbleCount then 
                currentSet
            -- Otherwise, we begin generating boba for a single row
            else 
                let
                    -- 1) Calculate the width of the row 
                    -- The base is the cupWidth in the settings multiplied by the diameter of the boba
                    baseWidth = toFloat (settings.cupWidth * 2)
                    -- Then the additional width of the current row is given by twice the width of a triangle with height (rowNumber * 2) and angle cupSlope
                    extraWidth = 2 * ( toFloat rowNumber * 2) * (tan (degrees settings.cupSlope))
                    rowWidth = baseWidth + extraWidth
                    -- 2) Calculate the number of boba that will fit in this width
                    numBoba = floor (rowWidth / 2)
                    -- 3) Place boba in this row (starting from x = 0 - numBoba)
                    newBoba = 
                        List.range 0 (numBoba - 1) 
                        |> List.map (\i -> ( 0 - numBoba + i * 2 + 1, rowNumber * 2))
                        |> List.take (settings.bubbleCount - Set.size currentSet)
                        |> Set.fromList
                in
                -- And we now add these to the accumulator of current boba and recurse to the next row
                recursivelyFillRow (rowNumber + 1) (Set.union currentSet newBoba)
    in
    recursivelyFillRow 0 Set.empty


{-| Create the initial game data given the settings.
-}
init : Settings -> ( Game, Cmd Msg )
init settings =
    let
        initialGame =
            { settings = settings
            , status = Playing
            , turn = Player1
            , cup = initCup settings 
            , debug = []
            }
    in
    ( initialGame, Cmd.none )



--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = Sip Coord


{-| Apply a move to a game state, returning a new game state.
-}
applyMove : Move -> Game -> Game
applyMove move game =
    case move of
        Sip (x,y) ->
            let newGame = sipAtLocation (x, y) game
            in
            if Set.size newGame.cup == 0 then
                { newGame | status = Complete (Winner newGame.turn) }
            else
                { newGame | turn = opponent game.turn }


{-| Helper function to sip at a location
Strategy:
1. Get the bobas in range of the sip, convert to a list
2. Recurse through the list, remove corresponding boba from the cup, pass the new cup to the next recursion
-}
sipAtLocation: Coord -> Game -> Game
sipAtLocation sip game =
    let
        bobasInRange = List.sortWith flippedComparison (Set.toList (Set.filter (\c -> (distance c sip) <= 2) game.cup))
        recursiveSip bobas cup = 
            case bobas of
                [] ->
                    cup
                (x,y)::xs ->
                    recursiveSip xs (Set.remove (dropAtLocation (x,y) cup) cup)
    in
        { game | cup = recursiveSip bobasInRange game.cup, debug = bobasInRange}

{-| Helper function to drop bubbles from above to fill in the gap
-}
dropAtLocation : Coord -> Set Coord -> Coord
dropAtLocation (x, y) cup =
    if (Set.member (x, y + 2) cup) then
        dropAtLocation (x, y + 2) cup
    else if (Set.member (x + 1, y + 2) cup) then
        dropAtLocation (x + 1, y + 2) cup
    else if (Set.member (x - 1, y + 2) cup) then
        dropAtLocation (x - 1, y + 2) cup
    else
        (x, y)

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
    = ClickedCell Coord
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
        ClickedCell coord ->
            let 
                nextState = applyMove (Sip coord) game
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
makeComputerMoveEasy game = Task.perform ReceivedComputerMove (Task.succeed (Sip (0,0))) 


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

{- View a single boba as an svg.  -}
viewBoba : Coord -> Svg Msg
viewBoba (x, y) = 
    let
        -- The y center needs to be adjusted for viewing (as it currently is the bottom of the boba)
        cx = toFloat x
        cy = toFloat y + 1.0 
        -- Make the radius slightly more than 1 to make the boba more packed
        radius = 1.1
    in
    
    Svg.circle2d 
        [ Svg.Attributes.fill "black"]
        (Circle2d.atPoint (Point2d.pixels cx cy) (Pixels.float radius))

{-| View clickable points on the grid -}
viewClickablePoints : Game -> List (Svg Msg)
viewClickablePoints game = 
    let
        bobaList = Set.toList game.cup
        minX = 
            bobaList
            |> List.map Tuple.first
            |> List.minimum
            |> Maybe.withDefault 0
        maxX = 
            bobaList
            |> List.map Tuple.first
            |> List.maximum
            |> Maybe.withDefault 0
            
        minY = 0
        maxY = 
            bobaList
            |> List.map Tuple.second
            |> List.maximum
            |> Maybe.withDefault 0
        allCoords = 
            List.range minX maxX
            |> List.map (\x -> List.range minY maxY |> List.map (\y -> (x, y)))
            |> List.concat
        viewClickableCoord (x, y) = 
            Svg.rectangle2d
                [ Svg.Attributes.fill "transparent"
                , Svg.Attributes.class "clickable-point"
                , Svg.Events.onClick (ClickedCell (x, y)) ]
                (Rectangle2d.with { x1 = Pixels.pixels (toFloat x - 1.1), y1 = Pixels.pixels (toFloat y),  x2 = (Pixels.pixels (toFloat x - 1.1 + 2.2)), y2 = (Pixels.pixels (toFloat y + 2.2))} )
    in
    allCoords 
    |> List.map viewClickableCoord
    
    

{-| Actual game view
-}
viewCup : Game -> Html Msg
viewCup game =
    let
        -- SVG CONTAINER:
        -- Assume that the maximum number of rows in the cup is 10 
        maxRows = 10 
        -- Then the height of the cup is given by number of rows times the diameter of a boba 
        cupHeight = maxRows * 2
        
        -- TODO: Note you will probably need to increase the height of the viewbox to accommodate the straw eventually!
        -- For now, I will make it 5 units (but you might want to define this differently)
        strawHeight = 10
        totalHeight = cupHeight + strawHeight
        -- The width of the base of the cup is known in the settings
        -- The width of the viewbox must accommodate the width of the top of the cup 
        -- The width of the top can be found using trigonometry 
        baseWidth = toFloat game.settings.cupWidth * 2 
        -- Reduce the total width by the compress constant to make the boba more compressed
        totalWidth =  baseWidth + 2 * (toFloat maxRows * 2  * (tan (degrees game.settings.cupSlope))) 
        -- We can now set the viewbox height and width 
        -- Divide the width by two as we defined coordinate (0,0) to be the center of the base of the cup 
        -- Add a buffer value to prevent edges being cut off 
        buffer = 2
        viewBoxHeight = String.fromFloat (totalHeight + buffer)
        viewBoxWidth = String.fromFloat (totalWidth + buffer)
        viewBoxXStart = String.fromFloat (0 - totalWidth / 2 - buffer)
        viewBoxYStart = String.fromFloat (0 - buffer)
        viewBoxString = String.join " " [viewBoxXStart, viewBoxYStart, viewBoxWidth, viewBoxHeight]
        topLeftFrame = 
            Frame2d.atPoint (Point2d.pixels (buffer / 2) (totalHeight - buffer / 2))
                |> Frame2d.reverseY

        -- CUP TRAPEZIUM: 
        -- Drawing the cup (trapezium) as a 2D polygon:
        cupTrapezium = 
            Svg.polygon2d
                [ Svg.Attributes.stroke "black"
                , Svg.Attributes.fill "white" 
                , Svg.Attributes.strokeWidth "0.2"
                ]
                -- The vertices of the trapezium are relatively straightforward after calculating our viewbox
                (Polygon2d.singleLoop 
                    [Point2d.pixels (0 - baseWidth / 2) 0
                    , Point2d.pixels (baseWidth / 2) 0
                    , Point2d.pixels (totalWidth / 2) cupHeight
                    , Point2d.pixels (0 - totalWidth / 2) cupHeight ])

    in
    div
        [ id "cup-container" ]
        [ Svg.svg 
            [ id "cup"
            , Svg.Attributes.width "600"
            , Svg.Attributes.height "800"
            , Svg.Attributes.viewBox viewBoxString ] 
            [ Svg.relativeTo topLeftFrame
                ( Svg.g 
                    []
                    [ Svg.g [] [ cupTrapezium ]
                    , Svg.g [] (List.map viewBoba (Set.toList game.cup))
                    , Svg.g [] (viewClickablePoints game) ] )
            ] ]

--------------------------------------------------------------------------------
-- GAME HELPER FUNCTIONS
-- Helper functions to implement the game logic.
--------------------------------------------------------------------------------

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