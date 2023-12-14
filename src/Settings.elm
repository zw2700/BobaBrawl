module Settings exposing (..)

{-| This module handles everything on the Settings screen.

TODO: You will need to modify this file to add / remove settings for your game.

Adding/removing a setting is a 5-step process.
(I know it seems like a lot, but it is necessary so Elm can make static
guarantees at compile time about your Settings).

I've outlined the five steps below under SETTING DEFINITIONS.

-}

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



--------------------------------------------------------------------------------
-- SETTING DEFINITIONS
--
-- You can add / delete settings by modifying the following 5 steps:
-- 1. Define the data model for your settings and their types.
-- 2. Define the default values for your settings.
-- 3. Add a message type to update your settings.
-- 4. Define explicitly what happens to your settings when a message is received.
-- 5. Define a list of pickers for each setting you want to be able to change.
--
-- This should cover most of the basic use cases. If you need extra
-- customisation, you're welcome to edit the code below or delete everything
-- here and start from scratch.
--------------------------------------------------------------------------------


{-| STEP 1: Define the data model for your settings and their types.

Keep it simple: you probably don't as many settings as there are here
(you might only need 1 or 2). You'll have access to the data of this type
in your Game when the user clicks StartGame.

-}
type alias Settings =
    { playMode : PlayMode
    , gameDifficulty: Difficulty
    , computerDifficulty : Difficulty
    , cupWidth : Int
    , cupSlope: Float
    , bubbleCount: Int
    , maxForce: Float
    , player1Name : String
    , player1Colour : SimpleColour
    , player2Name : String
    , player2Colour : SimpleColour
    }


{-| STEP 2: Define the default values for your settings.

For simplicity's sake, every setting MUST have a default value.

-}
default : Settings
default =
    { playMode = PlayHumanVsHuman
    , gameDifficulty = Easy
    , computerDifficulty = Easy
    , cupWidth = 5 -- interpret this as the cup width in terms of number of boba in the bottom row
    , cupSlope = 10.0 -- interpret this as the degrees of slope off the vertical
    , bubbleCount = 24
    , maxForce = 3.0
    , player1Name = "Alice"
    , player1Colour = Red
    , player2Name = "Bob"
    , player2Colour = Blue
    }


{-| STEP 3: Add a message type to update your settings.

Your message type should have a payload attached (the new value for the
setting). This is typically the same type as your setting.

-}
type Msg
    = SetPlayMode PlayMode
    | SetGameDifficulty Difficulty
    | SetComputerDifficulty Difficulty
    | SetCupWidth Int
    | SetCupSlope Float
    | SetBubbleCount Int
    | SetMaxForce Float
    | SetPlayerName Player String
    | SetPlayerColour Player SimpleColour


{-| STEP 4: Define explicitly what happens to your settings when a message is received.

Handle each Msg case below. Most likely, you'll just update the settings record
with the new payload. You can see the implementations below for this.

-}
update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        SetPlayMode playMode ->
            { settings | playMode = playMode }

        SetGameDifficulty difficulty ->
            { settings | gameDifficulty = difficulty }

        SetComputerDifficulty difficulty ->
            { settings | computerDifficulty = difficulty }

        SetCupWidth width ->
            { settings | cupWidth = width }

        SetCupSlope slope ->
            { settings | cupSlope = slope }

        SetBubbleCount count ->
            { settings | bubbleCount = count }

        SetMaxForce force ->
            { settings | maxForce = force }

        SetPlayerName player name ->
            case player of
                Player1 ->
                    { settings | player1Name = name }

                Player2 ->
                    { settings | player2Name = name }

        SetPlayerColour player colour ->
            case player of
                Player1 ->
                    { settings | player1Colour = colour }

                Player2 ->
                    { settings | player2Colour = colour }



{-| STEP 5: Define a list of pickers for each setting you want to be able to change.

I've defined a bunch of helper functions for you to make things easier.

Helper functions include:

  - inputString (a small text input for the user to input a string)
  - inputFloat (a number input for floats)
  - inputInt (a number input for ints)
  - inputFloatRange (a range slider for floats)
  - inputIntRange (a range slider for ints)
  - pickChoiceButtons (a set of buttons for the user to pick from - good for small enums)
  - pickChoiceDropdown (a dropdown of options for the user to pick from)

Each function has it's own type defining what data it needs; see the HELPER
FUNCTIONS section.

You can customise this further if you so wish (see the HELPER FUNCTIONS section below).

-}
pickers : Settings -> List SettingPickerItem
pickers settings =
    let 
        forceLabel = if settings.gameDifficulty == Easy then "Force" else "Max Force"
    in
    [ pickChoiceDropdown
        { label = "Play Mode"
        , onSelect = SetPlayMode
        , toString = playModeToString
        , fromString = stringToPlaymode
        , current = settings.playMode
        , options = [ ( "Human vs Human", PlayHumanVsHuman ), ( "Me vs Computer", PlayMeVsComputer ), ( "Computer vs Me", PlayComputerVsMe ) ]
        }
    , pickChoiceButtons
        { label = "Game Difficulty"
        , onSelect = SetGameDifficulty
        , current = settings.gameDifficulty
        , options = [ ( "Easy", Easy ), ( "Hard", Hard ) ]
        }
    , pickChoiceButtons
        { label = "Computer Difficulty"
        , onSelect = SetComputerDifficulty
        , current = settings.computerDifficulty
        , options = [ ( "Easy", Easy ), ( "Hard", Hard ) ]
        }
    , inputIntRange
        { label = "Cup Width"
        , value = settings.cupWidth
        , min = 3
        , max = 10
        , onChange = SetCupWidth
        }
    , inputFloatRange
        { label = "Cup Slope"
        , value = settings.cupSlope
        , step = 0.1
        , min = 0.0
        , max = 45.0
        , onChange = SetCupSlope
        }
    , inputIntRange
        { label = "Bubble Count"
        , value = settings.bubbleCount
        , min = 5
        , max = 50
        , onChange = SetBubbleCount
        }
    , inputFloatRange
        { label = forceLabel
        , value = settings.maxForce
        , step = 0.5
        , min = 1.0
        , max = 5.0
        , onChange = SetMaxForce
        }
    , inputString
        { label = "Player 1 Name"
        , value = settings.player1Name
        , onChange = SetPlayerName Player1
        }
    , pickChoiceButtons
        { label = "Player 1 Colour"
        , onSelect = SetPlayerColour Player1
        , current = settings.player1Colour
        , options = [ ( "Red", Red ), ( "Green", Green ), ( "Blue", Blue ) ]
        }
    , inputString
        { label = "Player 2 Name"
        , value = settings.player2Name
        , onChange = SetPlayerName Player2
        }
    , pickChoiceButtons
        { label = "Player 2 Colour"
        , onSelect = SetPlayerColour Player2
        , current = settings.player2Colour
        , options = [ ( "Red", Red ), ( "Green", Green ), ( "Blue", Blue ) ]
        }
    ]

--------------------------------------------------------------------------------
-- SUPPORTING TYPES
-- A few custom types I've defined for my settings, as I wanted to represent
-- some of the choices as enums.
--------------------------------------------------------------------------------


{-| Play mode (i.e. human vs human, me vs AI or AI vs me) for the game.
-}
type PlayMode
    = PlayHumanVsHuman
    | PlayMeVsComputer
    | PlayComputerVsMe


{-| Basic function to convert a PlayMode to a String (for the option selector).
-}
playModeToString : PlayMode -> String
playModeToString playMode =
    case playMode of
        PlayHumanVsHuman ->
            "Human vs Human"

        PlayMeVsComputer ->
            "Me vs Computer"

        PlayComputerVsMe ->
            "Computer vs Me"


{-| Basic function to convert a String to a PlayMode, with a default.
-}
stringToPlaymode : String -> PlayMode
stringToPlaymode string =
    case string of
        "Human vs Human" ->
            PlayHumanVsHuman

        "Me vs Computer" ->
            PlayMeVsComputer

        "Computer vs Me" ->
            PlayComputerVsMe

        _ ->
            PlayHumanVsHuman


{-| Difficulty of the game & computer player.
-}
type Difficulty
    = Easy
    | Hard


{-| A simple type to represent three possible colours
-}
type SimpleColour
    = Red
    | Green
    | Blue


{-| Convert a colour to a string
-}
colourToString : SimpleColour -> String
colourToString colour =
    case colour of
        Red ->
            "red"

        Green ->
            "green"

        Blue ->
            "blue"

-- =============================================================================
-- =============================================================================
-- NOTE: YOU PROBABLY DON'T HAVE TO MODIFY ANYTHING BELOW THIS LINE
-- =============================================================================
-- =============================================================================
--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- If your use cases are covered by the basic types of settings above, you don't have to
-- edit any of the code below (it's boilerplate to make things easier for you).
-- However, if you need extra customisation, then you're welcome to edit it
-- if you know what you're doing (e.g. show a setting only in certain conditions,
-- or add extra specific styling to a setting).
--------------------------------------------------------------------------------
-- Helper functions to create Setting picker item types.
-- These are the functions you'll actually use to construct your pickers.
-- INPUT STRING


type alias InputStringConfig =
    { label : String
    , value : String
    , onChange : String -> Msg
    }


{-| A basic text box that allows the user to input a string.
-}
inputString : InputStringConfig -> SettingPickerItem
inputString data =
    InputString data



-- INPUT FLOAT


type alias InputFloatConfig =
    { label : String
    , value : Float
    , min : Float
    , max : Float
    , onChange : Float -> Msg
    }


{-| A basic box that allows the user to input a float.
-}
inputFloat : InputFloatConfig -> SettingPickerItem
inputFloat data =
    InputFloat data



-- INPUT INT


type alias InputIntConfig =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> Msg
    }


{-| A basic box that allows the user to input an int.
-}
inputInt : InputIntConfig -> SettingPickerItem
inputInt data =
    InputInt data



-- INPUT FLOAT RANGE


type alias InputFloatRangeConfig =
    { label : String
    , value : Float
    , step : Float
    , min : Float
    , max : Float
    , onChange : Float -> Msg
    }


{-| A range slider that allows the user to input a float.
-}
inputFloatRange : InputFloatRangeConfig -> SettingPickerItem
inputFloatRange data =
    InputFloatRange data



-- INPUT INT RANGE


type alias InputIntRangeConfig =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> Msg
    }


{-| A range slider that allows the user to input an int.
-}
inputIntRange : InputIntRangeConfig -> SettingPickerItem
inputIntRange data =
    InputIntRange data



-- PICK CHOICE BUTTONS


type alias PickChoiceButtonsGenericConfig enum =
    { label : String
    , onSelect : enum -> Msg
    , current : enum
    , options : List ( String, enum )
    }


{-| A set of buttons that allows the user to pick from a list of options.
-}
pickChoiceButtons : PickChoiceButtonsGenericConfig enum -> SettingPickerItem
pickChoiceButtons { label, onSelect, current, options } =
    PickChoiceButtons
        { label = label
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, onSelect = onSelect value, isSelected = value == current }) options
        }



-- PICK CHOICE DROPDOWN


type alias PickChoiceDropdownGenericConfig enum =
    { label : String
    , onSelect : enum -> Msg
    , toString : enum -> String
    , fromString : String -> enum
    , current : enum
    , options : List ( String, enum )
    }


{-| A dropdown that allows the user to pick from a list of options.
-}
pickChoiceDropdown : PickChoiceDropdownGenericConfig enum -> SettingPickerItem
pickChoiceDropdown { label, onSelect, toString, fromString, current, options } =
    PickChoiceDropdown
        { label = label
        , onSelect = fromString >> onSelect
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, value = toString value, isSelected = value == current }) options
        }



--------------------------------------------------------------------------------
-- PICKER TYPES
--------------------------------------------------------------------------------


{-| A type of a single item in a setting picker

Note: these are NOT constructed directly. Instead, there are specific helper
functions to construct each of these. The reason is because Elm's type
system is a bit limited, and we want to be able to have different types of Enums
stored as items - so my compromise is to use more generic helper functions to convert it
into these types instead.

-}
type SettingPickerItem
    = InputString InputStringConfig
    | InputFloat InputFloatConfig
    | InputInt InputIntConfig
    | InputFloatRange InputFloatRangeConfig
    | InputIntRange InputIntRangeConfig
    | PickChoiceButtons PickChoiceButtonsConfig
    | PickChoiceDropdown PickChoiceDropdownConfig


type alias PickChoiceOptionButton =
    { label : String
    , onSelect : Msg
    , isSelected : Bool
    }


type alias PickChoiceButtonsConfig =
    { label : String
    , options : List PickChoiceOptionButton
    }


type alias PickChoiceDropdownOption =
    { label : String
    , value : String
    , isSelected : Bool
    }


type alias PickChoiceDropdownConfig =
    { label : String
    , onSelect : String -> Msg
    , options : List PickChoiceDropdownOption
    }



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


{-| The view function for a single setting picker item.

Renders each item based on its type. You also have access to the
current settings in this function (as Settings) so can use that
information to make decisions on what to render as well.

-}
viewPickerItem : Settings -> SettingPickerItem -> Html Msg
viewPickerItem settings item =
    case item of
        InputString data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input [ class "setting-picker-item-input setting-picker-item-input-string", type_ "text", value data.value, onInput data.onChange ] []
                ]

        InputFloat data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-float"
                    , type_ "number"
                    , value (String.fromFloat data.value)
                    , Html.Attributes.min (String.fromFloat data.min)
                    , Html.Attributes.max (String.fromFloat data.max)
                    , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                    ]
                    []
                ]

        InputInt data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-int"
                    , type_ "number"
                    , value (String.fromInt data.value)
                    , Html.Attributes.min (String.fromInt data.min)
                    , Html.Attributes.max (String.fromInt data.max)
                    , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                    ]
                    []
                ]

        InputFloatRange data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-float-range"
                        , type_ "range"
                        , value (String.fromFloat data.value)
                        , Html.Attributes.min (String.fromFloat data.min)
                        , Html.Attributes.max (String.fromFloat data.max)
                        , step (String.fromFloat data.step)
                        , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromFloat data.value) ]
                    ]
                ]

        InputIntRange data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-int-range"
                        , type_ "range"
                        , value (String.fromInt data.value)
                        , Html.Attributes.min (String.fromInt data.min)
                        , Html.Attributes.max (String.fromInt data.max)
                        , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromInt data.value) ]
                    ]
                ]

        PickChoiceButtons data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input setting-picker-item-input-buttons" ]
                    (List.map
                        (\{ label, onSelect, isSelected } ->
                            button
                                [ class ("setting-picker-item-button setting-picker-item-button-" ++ String.replace " " "-" label)
                                , classList [ ( "selected", isSelected ) ]
                                , onClick onSelect
                                ]
                                [ text label ]
                        )
                        data.options
                    )
                ]

        PickChoiceDropdown data ->
            div [ class "setting-picker-item" ]
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , select [ class "setting-picker-item-input setting-picker-item-input-select", onInput data.onSelect ]
                    (List.map
                        (\optionData ->
                            option [ value optionData.value, selected optionData.isSelected ] [ text optionData.label ]
                        )
                        data.options
                    )
                ]


{-| View just the picker part of the settings
-}
viewPicker : Settings -> List SettingPickerItem -> Html Msg
viewPicker settings items =
    div [ id "settings-picker" ]
        (List.map (viewPickerItem settings) items)


{-| The function that views all settings which gets called from the Main application.
-}
view : Settings -> Html Msg
view settings =
    div [ id "settings" ]
        [ viewPicker settings (pickers settings)
        ]
