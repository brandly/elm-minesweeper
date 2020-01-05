module Bitmap exposing (Face(..), forCell, forChar, forFace)

import Element exposing (Element, px, styled)
import GameMode exposing (GameMode)
import Grid exposing (Cell, CellState(..))
import Html exposing (div)


forChar : Char -> Element msg
forChar n =
    let
        pos =
            case n of
                '0' ->
                    ( 0, 0 )

                '1' ->
                    ( -13, 0 )

                '2' ->
                    ( -26, 0 )

                '3' ->
                    ( -39, 0 )

                '4' ->
                    ( -52, 0 )

                '5' ->
                    ( -65, 0 )

                '6' ->
                    ( -78, 0 )

                '7' ->
                    ( -91, 0 )

                '8' ->
                    ( -104, 0 )

                '9' ->
                    ( -117, 0 )

                '-' ->
                    ( -130, 0 )

                _ ->
                    ( 0, 0 )
    in
    bitmap pos


type Face
    = Smile
    | Pressed
    | Surprised
    | Sad
    | Sunglasses
    | SetDifficultyFace


forFace : Face -> Element msg
forFace face =
    let
        pos =
            case face of
                Smile ->
                    ( 0, -55 )

                Pressed ->
                    ( -26, -55 )

                Surprised ->
                    ( -52, -55 )

                Sad ->
                    ( -78, -55 )

                Sunglasses ->
                    ( -104, -55 )

                SetDifficultyFace ->
                    ( -130, -55 )
    in
    bitmap pos


forCell : Int -> GameMode -> Cell -> Element msg
forCell neighbors mode cell =
    let
        mapNum n =
            case n of
                0 ->
                    ( 0, -23 )

                1 ->
                    ( -16, -23 )

                2 ->
                    ( -32, -23 )

                3 ->
                    ( -48, -23 )

                4 ->
                    ( -64, -23 )

                5 ->
                    ( -80, -23 )

                6 ->
                    ( -96, -23 )

                7 ->
                    ( -112, -23 )

                8 ->
                    ( -128, -23 )

                _ ->
                    ( 0, 0 )

        flag =
            ( -16, -39 )

        misflagged =
            ( -48, -39 )

        bomb =
            ( -64, -39 )

        redBomb =
            ( -32, -39 )

        pressed =
            ( 0, -23 )

        raised =
            ( 0, -39 )

        pos =
            case ( mode, cell.state, cell.bomb ) of
                ( _, Exposed, True ) ->
                    redBomb

                ( _, Exposed, False ) ->
                    mapNum neighbors

                ( GameMode.Lose, Flagged, True ) ->
                    flag

                ( GameMode.Lose, Flagged, False ) ->
                    misflagged

                ( GameMode.Lose, _, True ) ->
                    bomb

                ( _, Flagged, _ ) ->
                    flag

                -- all bombs are flagged after winning
                ( GameMode.Win, _, True ) ->
                    flag

                ( _, Initial, _ ) ->
                    if cell.active then
                        pressed

                    else
                        raised
    in
    bitmap pos


bitmap : ( Int, Int ) -> Element msg
bitmap ( x, y ) =
    styled div
        [ ( "background-image", "url('./images/minesweeper.png')" )
        , ( "background-position", px x ++ " " ++ px y )
        ]
