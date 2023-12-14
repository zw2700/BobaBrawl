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
import Browser.Dom as Dom
import Common exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
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
import Random exposing (..)
import Random.List exposing (..)
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

Cup is a set of coordinates indicating where boba are
Note: if it's not in the set, it's empty

-}
type alias Game =
    { settings : Settings
    , status : Status
    , turn : Player
    , cup : Set Coord
    , cupShape: Shape
    , bobasPerRow: List Int
    , currentForce: Float
    , mouseCoordinate: Coord
    , boardElement : Maybe Dom.Element
    , debug: List Int
    }


{-| Type for shape of the bottle.-}
type alias Shape = 
    {
          maxRows: Float
        , cupHeight: Float
        , strawHeight: Float
        , totalHeight: Float
        , baseWidth: Float
        , totalWidth: Float
    }

--------------------------------------------------------------------------------
-- Initialization Functions
--------------------------------------------------------------------------------

{-| Initialise the number of bobas per row.-}
init_bpr : Settings -> List Int
init_bpr settings = 
    List.range 0 9
        |> List.map (Settings.calculateRowCount settings.cupWidth settings.cupSlope)


{-| Initialise the set of boba in the cup. 
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
initCup : Settings -> List Int -> Set Coord
initCup settings bobasPerRow = 
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
                    numBoba = Maybe.withDefault 0 (List.Extra.getAt rowNumber bobasPerRow)
                    -- Place boba in this row (starting from x = 0 - numBoba)
                    newBoba = 
                        List.range 0 (numBoba - 1) 
                        |> List.map (\i -> ( 0 - numBoba + i * 2 + 1, rowNumber * 2 + 1))
                        |> List.take (settings.bubbleCount - Set.size currentSet)
                        |> Set.fromList
                in
                -- And we now add these to the accumulator of current boba and recurse to the next row
                recursivelyFillRow (rowNumber + 1) (Set.union currentSet newBoba)
    in
    recursivelyFillRow 0 Set.empty


{-| Initialise the shape of the cup.-}
init_cup_shape : Settings -> Shape
init_cup_shape settings = 
    let
        -- Height of cup is 10 by default. Can't be changed in this version.
        maxRows = 10 
        cupHeight = maxRows * 2
        
        -- Height of straw is 10 by default. Can't be changed in this version.
        strawHeight = 10
        totalHeight = cupHeight + strawHeight
        -- The width of the base of the cup is known in the settings
        -- The width of the viewbox must accommodate the width of the top of the cup 
        -- The width of the top can be found using trigonometry 
        baseWidth = toFloat settings.cupWidth * 2 
        -- Reduce the total width by the compress constant to make the boba more compressed
        totalWidth =  baseWidth + 2 * (toFloat maxRows * 2  * (tan (degrees settings.cupSlope))) 
    in
    {
        maxRows = maxRows
        , cupHeight = cupHeight
        , strawHeight = strawHeight
        , totalHeight = totalHeight
        , baseWidth = baseWidth
        , totalWidth = totalWidth
    }


{-| Create the initial game data given the settings.
-}
init : Settings -> ( Game, Cmd Msg )
init settings =
    let
        bpr = init_bpr settings
        initialGame =
            { settings = settings
            , status = Playing
            , turn = Player1
            , cup = initCup settings bpr
            , cupShape = init_cup_shape settings
            , bobasPerRow = bpr
            , currentForce = settings.maxForce
            , mouseCoordinate = (0,0)
            , boardElement = Nothing
            , debug = [] -- for debug purposes
            }

        getBoardTask =
            Task.attempt ReceivedBoardElement (Dom.getElement "trapezoid")

        initialTask =
            case settings.playMode of
                PlayComputerVsMe ->
                    Cmd.batch
                        [ getBoardTask
                        , Task.perform (\_ -> PauseThenMakeComputerMove) (Process.sleep 500)
                        ]

                _ ->
                    getBoardTask
    in
    (initialGame, initialTask)


--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = Sip Coord Float


{-| Apply a move to a game state, returning a new game state.
-}
applyMove : Move -> Game -> Game
applyMove move game =
    case move of
        Sip (x,y) force ->
            let newGame = sipAtLocation (x, y) force game
            in
            if Set.size newGame.cup == 0 then
                { newGame | status = Complete (Winner newGame.turn) }
            else
                { newGame | turn = opponent game.turn }


{-| Complex helper function to sip at a location that allows animation
Strategy:
1. Get the bobas in range of the sip, convert to a list, remove them
2. Recursively drop bobas in the whole bottle, starting from the bottom
-}
sipAtLocation: Coord -> Float -> Game -> Game
sipAtLocation sip force game =
    let
        bobasInRange = sortByWith Tuple.second descending (Set.toList (Set.filter (\c -> (distance c sip) <= force) game.cup))
        recursiveSip bobas cup = 
            case bobas of
                [] ->
                    cup
                (x,y)::xs ->
                    recursiveSip xs (Set.remove (x,y) cup)
    in
        { game | cup = recursiveSip bobasInRange game.cup}

{-| Helper function to drop a specific row of bobas to fill gap below recursively
-}
drop : Game -> Int -> Game
drop game rowNumber =
    let
        bobaList = List.filter (\(x,y) -> y==(rowNumber*2+1)) (Set.toList game.cup)
        recursiveDropBoba (x,y) cup = 
            let 
                rowCount = (Maybe.withDefault 0 (List.Extra.getAt (floor (toFloat (y-2) / 2)) game.bobasPerRow))
            in
            if y > 0 then
                if     (Set.member (x,y-2) cup == False) 
                    && (Set.member (x+1,y-2) cup == False) 
                    && (Set.member (x-1,y-2) cup == False) 
                    && (abs (x)) < rowCount then
                    recursiveDropBoba (x,y-2) (Set.insert (x,y-2) (Set.remove (x,y) cup))
                else if (Set.member (x+1,y-2) cup == False) 
                     && (Set.member (x+2,y-2) cup == False) 
                     && (Set.member (x,y-2) cup == False) 
                     && (abs (x+1)) < rowCount then
                    recursiveDropBoba (x+1,y-2) (Set.insert (x+1,y-2) (Set.remove (x,y) cup))
                else if (Set.member (x-1,y-2) cup == False) 
                     && (Set.member (x-2,y-2) cup == False) 
                     && (Set.member (x,y-2) cup == False) 
                     && (abs (x-1)) < rowCount then
                    recursiveDropBoba (x-1,y-2) (Set.insert (x-1,y-2) (Set.remove (x,y) cup))
                else if (Set.member (x-2,y-2) cup == False) 
                     && (Set.member (x-3,y-2) cup == False) 
                     && (Set.member (x-1,y-2) cup == False) 
                     && (abs (x-2)) < rowCount then
                    recursiveDropBoba (x-2,y-2) (Set.insert (x-2,y-2) (Set.remove (x,y) cup))
                else if (Set.member (x+2,y-2) cup == False) 
                     && (Set.member (x+3,y-2) cup == False) 
                     && (Set.member (x+1,y-2) cup == False) 
                     && (abs (x+2)) < rowCount then
                    recursiveDropBoba (x+2,y-2) (Set.insert (x+2,y-2) (Set.remove (x,y) cup))
                else
                    cup
            else
                cup

        recursiveDrop bobas cup = 
            case bobas of
                [] ->
                    cup
                (x,y)::xs ->
                    recursiveDrop xs (recursiveDropBoba (x,y) cup)
    in
        { game | cup = recursiveDrop bobaList game.cup }

{-| The type of mouse movement data
-}
type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    }

{-| Decode mouse move data on mouse move
-}
mouseMoveDecoder : Decoder MouseMoveData
mouseMoveDecoder =
    Decode.map2 MouseMoveData
        (Decode.at [ "clientX" ] Decode.int)
        (Decode.at [ "clientY" ] Decode.int)

{-| Convert mouse move data to a coordinate
-}
mouseMoveDataToCoord : Game -> Dom.Element -> MouseMoveData -> Coord
mouseMoveDataToCoord game boardElement data =
    let
        x =
            data.offsetX

        y =
            data.offsetY

        dx =
            toFloat x - boardElement.element.x

        dy =
            toFloat y - boardElement.element.y

        height =
            boardElement.element.height

        width =
            boardElement.element.width

        rowCount = 
            20 - ceiling (dy / (height / 20))

        bobaWidth = 
            width  / game.cupShape.totalWidth

        rowIndex = 
            let 
                sideWidth = ((dx - (width / 2) - bobaWidth / 2) / bobaWidth)
            in
            if sideWidth > 0 then
                floor sideWidth
            else
                ceiling sideWidth

        coord =
            (rowIndex, rowCount)
    in
    coord

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
    | SetForce Float
    | WaitForBobaDrop Int
    | MouseMoved MouseMoveData
    | ReceivedBoardElement (Result Dom.Error Dom.Element)
    | ResizedWindow Int Int
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
                nextState = applyMove (Sip coord game.currentForce) game
            in
            case nextState.status of
                Playing ->
                    nextState
                        |> withCmd (Task.perform (\_ -> WaitForBobaDrop 0) (Process.sleep 100))

                Complete _ ->
                    nextState
                        |> withCmd Cmd.none

        SetForce force ->
            { game | currentForce = force }
                |> withCmd Cmd.none

        WaitForBobaDrop rowNumber ->
            case rowNumber of
                10 ->
                    case game.settings.playMode of
                        PlayHumanVsHuman ->
                            game
                                |> withCmd Cmd.none

                        PlayComputerVsMe ->
                            if game.turn == Player1 then
                                game
                                    |> withCmd (Task.perform (\_ -> PauseThenMakeComputerMove) (Process.sleep 500))
                            else
                                game
                                    |> withCmd Cmd.none

                        PlayMeVsComputer ->
                            if game.turn == Player1 then
                                game
                                    |> withCmd Cmd.none
                            else
                                game
                                    |> withCmd (Task.perform (\_ -> PauseThenMakeComputerMove) (Process.sleep 500))
                _ ->
                    drop game rowNumber 
                        |> withCmd (Task.perform (\_ -> WaitForBobaDrop (rowNumber + 1)) (Process.sleep 100))

        MouseMoved data ->
            case game.boardElement of
                Just boardElement ->
                    { game | mouseCoordinate = mouseMoveDataToCoord game boardElement data }
                        |> withCmd Cmd.none
                Nothing ->
                    game |> withCmd Cmd.none

        ReceivedBoardElement result ->
            case result of
                Ok element ->
                    { game | boardElement = Just element }
                        |> withCmd Cmd.none
                Err _ ->
                    ( game, Cmd.none )

        ResizedWindow width height ->
            game
                |> withCmd (Task.attempt ReceivedBoardElement (Dom.getElement "trapezoid"))

        PauseThenMakeComputerMove ->
            case game.settings.computerDifficulty of
                Settings.Easy ->
                    game |> withCmd (makeComputerMoveEasy game)

                Settings.Hard ->
                    game |> withCmd (makeComputerMoveHard game)

        ReceivedComputerMove move ->
            applyMove move game
                |> withCmd (Task.perform (\_ -> WaitForBobaDrop 0) (Process.sleep 100))

        NoOp ->
            game
                |> withCmd Cmd.none


--------------------------------------------------------------------------------
-- COMPUTER: EASY PLAYER
--------------------------------------------------------------------------------

{-| Logic for an "easy" computer player.

Random sips.

Your computer player function takes a game and returns a move, which you
then wrap in ReceivedComputerMove to create a Cmd Msg.

-}
makeComputerMoveEasy : Game -> Cmd Msg
makeComputerMoveEasy game = 
    let 
        bobaList = Set.toList game.cup
        coord = Random.List.choose bobaList
            |> Random.map
                (\maybeCoord ->
                    maybeCoord
                        |> first
                        |> Maybe.withDefault (0,0)
                )
        force = 
            case game.settings.gameDifficulty of
                Easy ->
                    Random.float game.settings.maxForce game.settings.maxForce
                Hard ->
                    Random.float 1 game.settings.maxForce
    in
        Random.map2 Sip coord force
            |> Random.generate ReceivedComputerMove

{-| Look one step into the future, makes sure that opponent cannot make a move that will cause it to lose
Strategy: 
1. Recursively generate random sips
2. Generate game state after move
3. If the game is complete, then return the move
4. Otherwise, calculate centroids of remaining bobas, calculate its radius, evaluate if radius is smaller than maxForce, if so, then recurse
-}
makeComputerMoveHard : Game -> Cmd Msg
makeComputerMoveHard game = 
    let 
        bobaList = Set.toList game.cup
        minForce = if game.settings.gameDifficulty == Easy then game.settings.maxForce else 1
        recursiveHelper bobaIndex force = 
            if bobaIndex >= List.length bobaList then
                makeComputerMoveEasy game
            else if force < minForce then
                recursiveHelper (bobaIndex + 1) game.settings.maxForce
            else
                let 
                    coord = (List.Extra.getAt bobaIndex bobaList) |> Maybe.withDefault (0,0)
                    nextGame = sipAtLocation coord force game
                in
                    if evaluateMove nextGame then
                        Task.perform ReceivedComputerMove (Task.succeed (Sip coord force))
                    else
                        recursiveHelper bobaIndex (force - 0.5)
    in
        recursiveHelper 0 game.settings.maxForce

{- Helper method to evaluate a move by calculating if opponent can finish the game -}
evaluateMove: Game -> Bool
evaluateMove game = 
    if Set.size game.cup == 0 then
        True
    else
        let 
            centroid = 
                Set.foldl 
                    (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) 
                    (0, 0) 
                    game.cup
                |> (\(x, y) -> (toFloat x / toFloat (Set.size game.cup), toFloat y / toFloat (Set.size game.cup)))
                |> (\(x, y) -> (round x, round y))
            radius = 
                Set.map 
                    (\(x1, y1) -> distance (x1, y1) centroid) 
                    game.cup
        in
            (radius |> Set.foldl Basics.max 0) > game.settings.maxForce


--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------


{-| The main view function that gets called from the Main application.

Essentially, takes a game and projects it into a HTML interface where Messages
can be sent from.

-}
view : Game -> Html Msg
view game =
    div [ id "game-screen-container" ]
        [ div [ id "game-header" ] [ viewStatus game ]
        , div [ id "game-main" ] [ viewCup game ]
        , viewForcePicker game
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
        cy = toFloat y
        -- Make the radius slightly more than 1 to make the boba more packed
        radius = 1.1
    in
    
    Svg.circle2d 
        [ Svg.Attributes.fill "black"]
        (Circle2d.atPoint (Point2d.pixels cx cy) (Pixels.float radius))

    
{-| View clickable points on the grid -}
viewClickablePoints : Game -> Coord -> List (Svg Msg)
viewClickablePoints game coord = 
    let
        withinCup (x, y) = 
            let
                rowWidth = toFloat (Maybe.withDefault 0 (List.Extra.getAt (floor (toFloat (y) / 2)) game.bobasPerRow))
            in
            (toFloat (x)) > (0 - rowWidth) && (toFloat (x)) < (rowWidth) && (toFloat y) > 0 && (toFloat y) < game.cupShape.cupHeight

        withinForceField (x, y) =
            let
                forceField = List.map (distance (x, y)) (Set.toList game.cup) 
            in
            forceField
                |> List.minimum
                |> Maybe.withDefault 0
                |> (>=) game.currentForce

        colour =
            case game.status of
                Complete (Winner Player1) ->
                    game.settings.player1Colour |> Settings.colourToString

                Complete (Winner Player2) ->
                    game.settings.player2Colour |> Settings.colourToString

                Playing ->
                    currentColour game |> Settings.colourToString

        viewClickableCoord (x, y) = 
            Svg.g [Svg.Attributes.class "clickable-point-container"]
            [ Svg.circle2d
                [ Svg.Attributes.fill colour
                , Svg.Attributes.class "clickable-point" 
                ]
                (Circle2d.atPoint (Point2d.pixels (toFloat x) (toFloat y)) (Pixels.float game.currentForce))
            , Svg.polygon2d
                [ Svg.Attributes.fill colour
                , Svg.Attributes.class "straw"]
                (Polygon2d.singleLoop
                    [Point2d.pixels (toFloat (x-1)) (toFloat y)
                    , Point2d.pixels (toFloat (x+1)) (toFloat (y-1))
                    , Point2d.pixels (toFloat (x+1)) game.cupShape.totalHeight
                    , Point2d.pixels (toFloat (x-1)) game.cupShape.totalHeight ])]
    in
    if withinCup coord && withinForceField coord then
        [viewClickableCoord coord]
    else
        []
    

{-| Actual game view
-}
viewCup : Game -> Html Msg
viewCup game =
    let
        -- We can now set the viewbox height and width 
        -- Divide the width by two as we defined coordinate (0,0) to be the center of the base of the cup 
        -- Add a buffer value to prevent edges being cut off 
        buffer = 2
        viewBoxHeight = String.fromFloat (game.cupShape.totalHeight + buffer)
        viewBoxWidth = String.fromFloat (game.cupShape.totalWidth + buffer)
        viewBoxXStart = String.fromFloat (0 - game.cupShape.totalWidth / 2 - buffer)
        viewBoxYStart = String.fromFloat (0 - buffer)
        viewBoxString = String.join " " [viewBoxXStart, viewBoxYStart, viewBoxWidth, viewBoxHeight]
        topLeftFrame = 
            Frame2d.atPoint (Point2d.pixels (buffer / 2) (game.cupShape.totalHeight - buffer / 2))
                |> Frame2d.reverseY

        -- CUP TRAPEZIUM: 
        -- Drawing the cup (trapezium) as a 2D polygon:
        cupTrapezium = 
            Svg.polygon2d
                [ Svg.Attributes.stroke "black"
                , Svg.Attributes.fill "white" 
                , Svg.Attributes.strokeWidth "0.2"
                , id "trapezoid"
                ]
                -- The vertices of the trapezium are relatively straightforward after calculating our viewbox
                (Polygon2d.singleLoop 
                    [Point2d.pixels (0 - game.cupShape.baseWidth / 2) 0
                    , Point2d.pixels (game.cupShape.baseWidth / 2) 0
                    , Point2d.pixels (game.cupShape.totalWidth / 2) game.cupShape.cupHeight
                    , Point2d.pixels (0 - game.cupShape.totalWidth / 2) game.cupShape.cupHeight ])

    in
    div
        [ id "cup-container", on "mousemove" (Decode.map MouseMoved mouseMoveDecoder)]
        [ Svg.svg 
            [ id "cup"
            , Svg.Attributes.width "600"
            , Svg.Attributes.viewBox viewBoxString 
            , Svg.Events.onClick (ClickedCell game.mouseCoordinate)
            ] 
            [ Svg.relativeTo topLeftFrame
                ( Svg.g 
                    []
                    [ Svg.g [] [ cupTrapezium ]
                    , Svg.g [] (List.map viewBoba (Set.toList game.cup))
                    , Svg.g [] (viewClickablePoints game game.mouseCoordinate)] )
            ] ]

viewForcePicker : Game -> Html Msg
viewForcePicker game = 
    if game.settings.gameDifficulty == Easy then
        div [ class "force-text-easy" ] [ text ("Force is fixed at " ++ String.fromFloat game.currentForce ++ " for easy mode.") ]
    else
        div [ class "setting-picker-item" ]
            [ label [ class "force-text-hard" ] [ text "Force" ]
            , div [ class "setting-picker-item-input-container" ]
                [ input
                    [ class "setting-picker-item-input setting-picker-item-input-float-range"
                    , type_ "range"
                    , value (String.fromFloat game.currentForce)
                    , Html.Attributes.min (String.fromFloat 1.0)
                    , Html.Attributes.max (String.fromFloat game.settings.maxForce)
                    , Html.Attributes.step (String.fromFloat 0.5)
                    , onInput (String.toFloat >> Maybe.withDefault 0.0 >> SetForce)
                    ]
                    []
                , div [ class "setting-picker-item-input-value" ] [ text (String.fromFloat game.currentForce) ]
                ]
            ]

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