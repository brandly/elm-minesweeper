module Main exposing (..)

import Html exposing (Html, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseEnter, onMouseOut, onMouseUp)


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
    | Active
    | Exposed


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
    , activeCell : Maybe Cell
    , pressingFace : Bool
    }


fromDimensions : Int -> Int -> Grid
fromDimensions width height =
    let
        makeColumn : Int -> Column
        makeColumn x =
            List.range 1 height
                |> List.map (\y -> Cell x y Pristine False)
    in
    List.range 1 width
        |> List.map makeColumn


withBombCount : Int -> Grid -> Grid
withBombCount count grid =
    let
        addBomb : Grid -> Grid
        addBomb =
            findEmptyCell grid
                |> updateCell (\cell -> { cell | bomb = True })
    in
    if totalBombs grid < count then
        withBombCount
            count
            (addBomb grid)
    else
        grid


withBombPairs : List ( Int, Int ) -> Grid -> Grid
withBombPairs pairs grid =
    let
        head =
            case List.head pairs of
                Just head ->
                    head

                Nothing ->
                    ( -1, -1 )

        tail =
            case List.tail pairs of
                Just tail ->
                    tail

                Nothing ->
                    []

        addBomb : Grid -> Grid
        addBomb =
            findCellAtPair head grid
                |> updateCell (\cell -> { cell | bomb = True })
    in
    if List.length pairs > 0 then
        withBombPairs
            tail
            (addBomb grid)
    else
        grid


findCellAtPair : ( Int, Int ) -> Grid -> Cell
findCellAtPair ( x, y ) grid =
    grid |> findCell (\cell -> cell.x == x && cell.y == y)


findEmptyCell : Grid -> Cell
findEmptyCell grid =
    grid |> findCell (\cell -> not cell.bomb)


findCell : (Cell -> Bool) -> Grid -> Cell
findCell match grid =
    let
        defaultCell =
            Cell -1 -1 Pristine False
    in
    gridToCells grid |> findMatching defaultCell match


findMatching : a -> (a -> Bool) -> List a -> a
findMatching default match list =
    let
        matches =
            list |> List.filter match
    in
    case List.head matches of
        Just empty ->
            empty

        Nothing ->
            default


totalBombs : Grid -> Int
totalBombs grid =
    List.length <| List.filter .bomb (gridToCells grid)


gridToCells : Grid -> List Cell
gridToCells grid =
    List.concat grid


dreamboard : List ( Int, Int )
dreamboard =
    [ ( 6, 1 )
    , ( 7, 1 )
    , ( 9, 1 )
    , ( 12, 1 )
    , ( 14, 1 )
    , ( 13, 2 )
    , ( 14, 2 )
    , ( 15, 2 )
    , ( 16, 2 )
    , ( 1, 3 )
    , ( 2, 3 )
    , ( 10, 3 )
    , ( 1, 5 )
    , ( 1, 6 )
    , ( 11, 6 )
    , ( 1, 7 )
    , ( 10, 7 )
    , ( 16, 7 )
    , ( 2, 8 )
    , ( 16, 8 )
    , ( 4, 9 )
    , ( 5, 9 )
    , ( 9, 9 )
    , ( 16, 9 )
    , ( 6, 10 )
    , ( 7, 10 )
    , ( 8, 10 )
    , ( 7, 11 )
    , ( 12, 11 )
    , ( 10, 12 )
    , ( 2, 13 )
    , ( 12, 13 )
    , ( 11, 14 )
    , ( 16, 14 )
    , ( 1, 15 )
    , ( 5, 15 )
    , ( 1, 16 )
    , ( 3, 16 )
    , ( 5, 16 )
    , ( 10, 16 )
    ]


initialGrid : Grid
initialGrid =
    withBombPairs
        dreamboard
        (fromDimensions 16 16)


initialModel : Model
initialModel =
    { grid = initialGrid
    , activeCell = Nothing
    , pressingFace = False
    }


type Msg
    = MouseUpCell Cell
    | PressDown Cell
    | PressingFace Bool
    | ClickFace


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ grid } as model) =
    case msg of
        MouseUpCell cell ->
            ( { model
                | grid = exposeCell cell model.grid
                , activeCell = Nothing
              }
            , Cmd.none
            )

        PressDown cell ->
            ( { model | activeCell = Just cell }, Cmd.none )

        PressingFace val ->
            ( { model | pressingFace = val }, Cmd.none )

        ClickFace ->
            ( { model | grid = initialGrid }, Cmd.none )


exposeCell : Cell -> Grid -> Grid
exposeCell cell grid =
    floodCells
        [ cell ]
        grid


floodCells : List Cell -> Grid -> Grid
floodCells toExpose grid =
    let
        cell =
            case List.head toExpose of
                Just cell ->
                    cell

                Nothing ->
                    Debug.crash ""

        state =
            case cell.state of
                Pristine ->
                    Exposed

                Active ->
                    Exposed

                _ ->
                    cell.state

        newGrid =
            updateCell
                (\cell -> { cell | state = state })
                cell
                grid

        moreToExpose : List Cell
        moreToExpose =
            List.concat [ toExpose, getNeighbors cell grid ]
                |> List.filter (\c -> not c.bomb)
                |> List.filter (\c -> c.state == Pristine)
                |> List.filter (\c -> not (c.x == cell.x && c.y == cell.y))
    in
    if cell.bomb then
        floodCells
            (gridToCells newGrid |> List.filter (\c -> not (c.state == Exposed)))
            newGrid
    else if List.length moreToExpose > 0 then
        floodCells moreToExpose newGrid
    else
        newGrid


updateCell : (Cell -> Cell) -> Cell -> Grid -> Grid
updateCell newCell cell grid =
    let
        replaceCell : Column -> Column
        replaceCell =
            List.map
                (\og ->
                    if og.x == cell.x && og.y == cell.y then
                        newCell cell
                    else
                        og
                )
    in
    grid |> List.map replaceCell


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    let
        background =
            styled div
                [ ( "box-sizing", "border-box" )
                , ( "min-height", "100vh" )
                , ( "width", "100%" )
                , ( "overflow", "hidden" )
                , ( "background-image", "url('https://www.hdwallpapers.in/walls/windows_xp_bliss-wide.jpg')" )
                ]

        frame =
            styled raisedDiv
                [ ( "display", "inline-block" )
                , ( "background-color", "#bdbdbd" )
                , ( "padding", "5px" )
                , ( "position", "absolute" )
                , ( "top", "48px" )
                , ( "left", "96px" )
                ]

        hasActiveCell : Bool
        hasActiveCell =
            case model.activeCell of
                Just cell ->
                    True

                Nothing ->
                    False
    in
    background
        []
        [ frame
            []
            [ viewHeader model.pressingFace hasActiveCell
            , viewGrid model.activeCell model.grid
            ]
        ]


viewHeader : Bool -> Bool -> Html Msg
viewHeader pressingFace hasActiveCell =
    let
        faceDiv : Element msg
        faceDiv =
            if pressingFace then
                insetDiv
            else
                raisedDiv

        header =
            styled insetDiv
                [ ( "display", "flex" )
                , ( "justify-content", "space-between" )
                , ( "align-items", "center" )
                , ( "height", "36px" )
                , ( "margin-bottom", "5px" )
                , ( "padding", "0 6px" )
                ]
    in
    header
        []
        [ viewDigits 0
        , faceDiv
            [ style
                [ ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                , ( "width", "24px" )
                , ( "height", "24px" )
                , ( "cursor", "default" )
                ]
            , onClick ClickFace
            , onMouseDown (PressingFace True)
            , onMouseUp (PressingFace False)
            , onMouseOut (PressingFace False)
            ]
            [ text
                (if hasActiveCell then
                    ":O"
                 else
                    ":)"
                )
            ]
        , viewDigits 0
        ]


viewDigits : Int -> Html Msg
viewDigits n =
    let
        frame =
            styled div [ ( "display", "inline-block" ), ( "background", "#000" ) ]

        digit =
            styled div
                [ ( "display", "inline-block" )
                , ( "width", "13px" )
                , ( "height", "23px" )
                ]

        --String.split
    in
    frame []
        [ digit [] []
        , digit [] []
        , digit [] []
        ]


viewGrid : Maybe Cell -> Grid -> Html Msg
viewGrid activeCell grid =
    let
        size =
            16

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

        markActive : Cell -> Cell
        markActive cell =
            case activeCell of
                Just active ->
                    if active == cell && cell.state == Pristine then
                        { cell | state = Active }
                    else
                        cell

                Nothing ->
                    cell

        hasActive : Maybe Cell -> Bool
        hasActive active =
            case active of
                Just cell ->
                    True

                Nothing ->
                    False

        renderCell =
            viewCell size (hasActive activeCell) grid

        viewColumn column =
            div
                [ style
                    [ ( "display", "inline-block" )
                    ]
                ]
                (column |> List.map (markActive >> renderCell))
    in
    insetDiv
        [ style
            [ ( "width", toString gridWidth ++ "px" )
            , ( "height", px gridHeight )
            ]
        ]
        (grid |> List.map viewColumn)


viewCell : Int -> Bool -> Grid -> Cell -> Html Msg
viewCell size downOnHover grid cell =
    let
        upStyle =
            [ ( "border", "2px solid #fff" )
            , ( "border-bottom-color", "#7b7b7b" )
            , ( "border-right-color", "#7b7b7b" )
            ]

        downStyle =
            [ ( "border-left", "1px solid #838383" )
            , ( "border-top", "1px solid #838383" )
            ]

        makeCellDiv extension =
            styled div
                ([ ( "box-sizing", "border-box" )
                 , ( "width", px size )
                 , ( "height", px size )
                 , ( "font-size", "10px" )
                 , ( "text-align", "center" )
                 , ( "overflow", "hidden" )
                 , ( "cursor", "default" )
                 ]
                    ++ extension
                )

        cellStyle =
            case cell.state of
                Pristine ->
                    upStyle

                Exposed ->
                    downStyle

                Active ->
                    downStyle

                _ ->
                    []

        cellDiv =
            makeCellDiv cellStyle

        additionalEvents =
            if downOnHover then
                [ onMouseEnter (PressDown cell) ]
            else
                []

        count =
            neighborBombCount cell grid

        children =
            case cell.state of
                Exposed ->
                    if cell.bomb then
                        [ text "*" ]
                    else if count > 0 then
                        [ text <| toString count ]
                    else
                        []

                _ ->
                    []
    in
    cellDiv
        ([ onMouseUp (MouseUpCell cell)
         , onMouseDown (PressDown cell)
         ]
            ++ additionalEvents
        )
        children


neighborBombCount : Cell -> Grid -> Int
neighborBombCount cell grid =
    List.length <| List.filter .bomb <| getNeighbors cell grid


getNeighbors : Cell -> Grid -> List Cell
getNeighbors cell grid =
    gridToCells grid |> List.filter (isNeighbor cell)


isNeighbor : Cell -> Cell -> Bool
isNeighbor a b =
    abs (a.x - b.x) <= 1 && abs (a.y - b.y) <= 1


px : Int -> String
px x =
    toString x ++ "px"


type alias Element msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


insetDiv : Element msg
insetDiv =
    styled div
        [ ( "border", "2px solid #7b7b7b" )
        , ( "border-bottom-color", "#fff" )
        , ( "border-right-color", "#fff" )
        ]


raisedDiv : Element msg
raisedDiv =
    styled div
        [ ( "border", "2px solid #7b7b7b" )
        , ( "border-top-color", "#fff" )
        , ( "border-left-color", "#fff" )
        ]


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el ([ style css ] ++ attrs) children
