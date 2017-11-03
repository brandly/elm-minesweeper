module Main exposing (..)

import Html exposing (Html, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseUp)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type CellState
    = Pristine
    | Flagged
    | Pressed
    | Empty
    | Neighbor Int


type alias Cell =
    { x : Int
    , y : Int
    , state : CellState
    , bomb : Bool
    }


type alias Column =
    List Cell


type alias Grid =
    List Column


type alias Model =
    { grid : Grid
    }


fromDimensions : Int -> Int -> Grid
fromDimensions width height =
    let
        makeColumn : Int -> Column
        makeColumn x =
            List.map (\y -> Cell x y Pristine False) (List.range 1 height)
    in
    List.map (\x -> makeColumn x) (List.range 1 width)



--withBombs : Int -> Grid -> Grid
--withBombs count grid = grid
--findEmptyCell : Grid -> Cell
--findEmptyCell grid = ...


initialModel : Model
initialModel =
    { grid = fromDimensions 16 16 }


type Msg
    = MouseUpCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseUpCell cell ->
            ( { model | grid = updateCell cell model.grid }, Cmd.none )


updateCell cell grid =
    case cell.state of
        Pristine ->
            setCellState
                Empty
                cell
                grid

        _ ->
            grid


setCellState : CellState -> Cell -> Grid -> Grid
setCellState state cell grid =
    let
        newCell =
            { cell | state = state }

        replaceCell col =
            List.map
                (\og ->
                    if og.x == cell.x && og.y == cell.y then
                        newCell
                    else
                        og
                )
                col
    in
    List.map
        (\col -> replaceCell col)
        grid


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    div [ style [ ( "box-sizing", "border-box" ) ] ]
        [ h1 [] [ text "~ minesweeper ~" ]
        , viewGrid model.grid
        , pre [] [ text (toString model.grid) ]
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    let
        size =
            16

        px x =
            toString x ++ "px"

        gridWidth =
            size * List.length grid

        columnHeight =
            case List.head grid of
                Just column ->
                    List.length column

                Nothing ->
                    0

        gridHeight =
            size * columnHeight

        upStyle =
            [ ( "border", "2px solid #ffffff" )
            , ( "border-bottom-color", "#7b7b7b" )
            , ( "border-right-color", "#7b7b7b" )
            ]

        downStyle =
            [ ( "border-right", "1px solid #838383" )
            , ( "border-bottom", "1px solid #838383" )
            ]

        makeCellDiv extension =
            styled div
                ([ ( "box-sizing", "border-box" )
                 , ( "width", px size )
                 , ( "height", px size )
                 , ( "font-size", "10px" )
                 , ( "text-align", "center" )
                 , ( "overflow", "hidden" )
                 ]
                    ++ extension
                )

        viewColumn column =
            div
                [ style
                    [ ( "display", "inline-block" )
                    ]
                ]
                (List.map
                    (\cell ->
                        let
                            cellStyle =
                                case cell.state of
                                    Pristine ->
                                        upStyle

                                    Empty ->
                                        downStyle

                                    _ ->
                                        []

                            cellDiv =
                                makeCellDiv cellStyle
                        in
                        cellDiv
                            [ onMouseUp (MouseUpCell cell) ]
                            (if cell.bomb then
                                [ text "*" ]
                             else
                                []
                            )
                    )
                    column
                )
    in
    div
        [ style
            [ ( "background-color", "#bdbdbd" )
            , ( "width", toString gridWidth ++ "px" )
            , ( "height", px gridHeight )
            ]
        ]
        (List.map
            viewColumn
            grid
        )


type alias Element msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el ([ style css ] ++ attrs) children
