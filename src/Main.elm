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


withBombs : Int -> Grid -> Grid
withBombs count grid =
    let
        addBomb : Grid -> Grid
        addBomb =
            findEmptyCell grid
                |> updateCell (\cell -> { cell | bomb = True })
    in
    if totalBombs grid < count then
        withBombs
            count
            (addBomb grid)
    else
        grid


findEmptyCell : Grid -> Cell
findEmptyCell grid =
    let
        empties =
            gridToCells grid
                |> List.filter (\cell -> not cell.bomb)
    in
    case List.head empties of
        Just empty ->
            empty

        Nothing ->
            Debug.crash ""


totalBombs : Grid -> Int
totalBombs grid =
    List.length <| List.filter .bomb (gridToCells grid)


gridToCells : Grid -> List Cell
gridToCells grid =
    List.concat grid


initialGrid : Grid
initialGrid =
    withBombs 40 (fromDimensions 16 16)


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
                | grid =
                    exposeCell cell model.grid
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

        neighbors : List Cell
        neighbors =
            getNeighbors cell grid

        moreToExpose : List Cell
        moreToExpose =
            List.concat [ toExpose, neighbors ]
                |> List.filter (\c -> not c.bomb)
                |> List.filter (\c -> c.state == Pristine)
                |> List.filter (\c -> not (c.x == cell.x && c.y == cell.y))
    in
    if List.length moreToExpose > 0 then
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
                [ ( "width", "24px" )
                , ( "height", "24px" )
                , ( "text-align", "center" )
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
