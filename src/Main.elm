module Main exposing (..)

import Html exposing (Html, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseEnter, onMouseOut, onMouseUp)
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Cell =
    { x : Int
    , y : Int
    , active : Bool
    , exposed : Bool
    , flagged : Bool
    , bomb : Bool
    }


type alias Column =
    List Cell


type alias Grid =
    List Column


type GameMode
    = Start
    | Play
    | Win
    | Lose


type alias Model =
    { grid : Grid
    , activeCell : Maybe Cell
    , pressingFace : Bool
    , bombCount : Int
    , time : Int
    , mode : GameMode
    }


fromDimensions : Int -> Int -> Grid
fromDimensions width height =
    let
        makeColumn : Int -> Column
        makeColumn x =
            List.range 1 height
                |> List.map (\y -> Cell x y False False False False)
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
            Cell -1 -1 False False False False
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
    , bombCount = 40
    , time = 0
    , mode = Start
    }


type Msg
    = MouseUpCell Cell
    | PressDown Cell
    | PressingFace Bool
    | ClickFace
    | TimeSecond Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ grid } as model) =
    case msg of
        MouseUpCell cell ->
            let
                mode =
                    case model.mode of
                        Start ->
                            Play

                        _ ->
                            model.mode
            in
            ( { model
                | grid = exposeCell cell model.grid
                , activeCell = Nothing
                , mode = mode
              }
            , Cmd.none
            )

        PressDown cell ->
            ( { model | activeCell = Just cell }, Cmd.none )

        PressingFace val ->
            ( { model | pressingFace = val }, Cmd.none )

        ClickFace ->
            ( { model | grid = initialGrid, time = 0, mode = Start }, Cmd.none )

        TimeSecond _ ->
            ( { model | time = model.time + 1 }, Cmd.none )


exposeCell : Cell -> Grid -> Grid
exposeCell cell grid =
    floodCells [ cell ] grid


floodCells : List Cell -> Grid -> Grid
floodCells toExpose grid =
    let
        cell =
            case List.head toExpose of
                Just cell ->
                    cell

                Nothing ->
                    Debug.crash ""

        tail =
            toExpose |> List.filter (\c -> not (c.x == cell.x && c.y == cell.y))

        newGrid =
            updateCell
                (\cell -> { cell | exposed = True })
                cell
                grid

        additional =
            if neighborBombCount cell grid == 0 then
                getNeighbors cell grid
                    |> List.filter (\c -> not c.bomb)
                    |> List.filter (\c -> not c.exposed && not c.flagged)
            else
                []

        moreToExpose =
            if cell.bomb then
                gridToCells newGrid
                    |> List.filter (\c -> not c.exposed && c.bomb)
            else
                List.concat [ tail, additional ]
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
    case model.mode of
        Play ->
            Time.every Time.second TimeSecond

        _ ->
            Sub.none


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

        flaggedCount =
            gridToCells model.grid
                |> List.filter .flagged
                |> List.length
    in
    background
        []
        [ frame
            []
            [ viewHeader model.pressingFace hasActiveCell (model.bombCount - flaggedCount) model.time
            , viewGrid model.activeCell model.grid
            ]
        ]


viewHeader : Bool -> Bool -> Int -> Int -> Html Msg
viewHeader pressingFace hasActiveCell remainingBombs time =
    let
        faceDiv : Element msg
        faceDiv =
            if hasActiveCell then
                bitmapForFace Surprised
            else if pressingFace then
                bitmapForFace Pressed
            else
                bitmapForFace Smile

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
        [ viewDigits remainingBombs
        , faceDiv
            [ style
                [ ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                , ( "width", "26px" )
                , ( "height", "26px" )
                , ( "cursor", "default" )
                ]
            , onClick ClickFace
            , onMouseDown (PressingFace True)
            , onMouseUp (PressingFace False)
            , onMouseOut (PressingFace False)
            ]
            []
        , viewDigits time
        ]


viewDigits : Int -> Html Msg
viewDigits n =
    let
        frame =
            styled div [ ( "display", "inline-block" ), ( "background", "#000" ) ]

        digit el =
            styled el
                [ ( "display", "inline-block" )
                , ( "width", "13px" )
                , ( "height", "23px" )
                ]

        minLen n str =
            if String.length str < n then
                minLen n ("0" ++ str)
            else
                str

        str =
            minLen 3 (toString n)

        toInt str =
            case String.toInt str of
                Ok num ->
                    num

                Err _ ->
                    0

        children =
            String.split "" str
                |> List.map (toInt >> bitmapForInt >> digit >> (\c -> c [] []))
    in
    frame
        [ style
            [ ( "height", "23px" )
            , ( "border", "1px solid" )
            , ( "border-color", "#808080 #fff #fff #808080" )
            ]
        ]
        children


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
                    if active == cell && not cell.exposed && not cell.flagged then
                        { cell | active = True }
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
        count =
            neighborBombCount cell grid

        base =
            bitmapForCell count cell

        cellDiv =
            styled base
                [ ( "box-sizing", "border-box" )
                , ( "width", px size )
                , ( "height", px size )
                , ( "overflow", "hidden" )
                , ( "cursor", "default" )
                ]

        additionalEvents =
            if downOnHover then
                [ onMouseEnter (PressDown cell) ]
            else
                []
    in
    cellDiv
        ([ onMouseUp (MouseUpCell cell)
         , onMouseDown (PressDown cell)
         ]
            ++ additionalEvents
        )
        []


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


bitmapForInt : Int -> Element msg
bitmapForInt n =
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
                        Debug.crash ""
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


bitmapForFace : Face -> Element msg
bitmapForFace face =
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


bitmapForCell : Int -> Cell -> Element msg
bitmapForCell neighbors cell =
    let
        pos =
            if cell.exposed then
                if cell.bomb then
                    if cell.flagged then
                        ( -48, -39 )
                    else
                        ( -64, -39 )
                else
                    case neighbors of
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
            else if cell.flagged then
                ( -16, -39 )
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


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el ([ style css ] ++ attrs) children
