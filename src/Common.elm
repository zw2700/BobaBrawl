module Common exposing (..)

{-| This module contains code shared in Settings, Main and Game.

It's a good place to store the really basic types and functions.

Importantly, if you have a type/function that's used in both Settings.elm and Game.elm,
I strongly suggest putting it here (to avoid circular dependencies).

You can delete any of the functions currently defined here - I just thought
they'd be useful for a lot of different types of games. 

-}

import List.Extra exposing (..)

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------


{-| Basic type representation for a two player game.
-}
type Player
    = Player1
    | Player2


{-| The game either ends up with a winner or as a draw.
-}
type Outcome
    = Winner Player
    | Draw


{-| A game is either in progress of complete.
-}
type Status
    = Playing
    | Complete Outcome

{-| A type corresponding to an integer coordinate on a board.
-}
type alias Coord = (Int, Int)


--------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS
--------------------------------------------------------------------------------


{-| A convenience function for the opposite player.
-}
opponent : Player -> Player
opponent player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1

{-| Helper function to get value from a list of integers
-}
getListValue: List Int -> Int -> Int
getListValue arr index =
    Maybe.withDefault 0 (List.Extra.getAt index arr)


{-| Distance between two coordinates.
-}
distance : Coord -> Coord -> Float
distance (x1, y1) (x2, y2) =
    sqrt (toFloat ((x1 - x2) ^ 2 + (y1 - y2) ^ 2))