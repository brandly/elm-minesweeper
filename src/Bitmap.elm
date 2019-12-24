module Bitmap exposing (..)

import Element exposing (Element, px, styled)
import GameMode exposing (GameMode)
import Grid exposing (Cell, CellState(..))
import Html exposing (div)


forInt : Int -> Element msg
forInt n =
    let
        pos =
            if n >= 0 && n <= 9 then
                case n of
                    0 ->
                        ( 0, 0 )

                    1 ->
                        ( -13, 0 )

                    2 ->
                        ( -26, 0 )

                    3 ->
                        ( -39, 0 )

                    4 ->
                        ( -52, 0 )

                    5 ->
                        ( -65, 0 )

                    6 ->
                        ( -78, 0 )

                    7 ->
                        ( -91, 0 )

                    8 ->
                        ( -104, 0 )

                    9 ->
                        ( -117, 0 )

                    _ ->
                        Debug.log "Oops! Something went wrong. Don't worry, a team of (one) highly trained will is trying to work out what went wrong. Please do submit an issue in Github if you ever see this message."
                        (0,0)
                        
            else
                ( 0, 0 )
    in
    bitmap pos


type Face
    = Smile
    | Pressed
    | Surprised
    | Sad
    | Sunglasses


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

        pos =
            if cell.state == Exposed then
                if cell.bomb then
                    ( -32, -39 )
                else
                    mapNum neighbors
            else if mode == GameMode.Lose && cell.bomb then
                if cell.state == Flagged then
                    flag
                else
                    bomb
            else if mode == GameMode.Lose && not cell.bomb && cell.state == Flagged then
                misflagged
            else if cell.state == Flagged || cell.bomb && mode == GameMode.Win then
                flag
            else if cell.active then
                ( 0, -23 )
            else
                ( 0, -39 )
    in
    bitmap pos


bitmap : ( Int, Int ) -> Element msg
bitmap pos =
    let
        bg =
            (Tuple.first pos |> px) ++ " " ++ (Tuple.second pos |> px)
    in
    styled div
        [ ( "background-image", "url(https://raw.githubusercontent.com/joelbyrd/external-resources/master/images/minesweeper.png)" )
        , ( "background-position", bg )
        ]
