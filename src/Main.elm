module Main exposing (..)

import Array
import Grid exposing (Cell, Column, Grid)
import Html exposing (Html, div, h1, p, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseEnter, onMouseOut, onMouseUp, onWithOptions)
import Json.Decode as Json
import Random exposing (Seed)
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


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


initialGrid : Grid
initialGrid =
    Grid.fromDimensions 16 16


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
    | ToggleFlag Cell
    | PressingFace Bool
    | ClickFace
    | TimeSecond Time
    | ArmRandomCells (List Int)


generateRandomInts : Int -> Grid -> Cmd Msg
generateRandomInts bombCount grid =
    let
        available =
            grid |> Grid.filter (\c -> not c.bomb && not c.exposed)
    in
    Random.generate ArmRandomCells <|
        Random.list bombCount <|
            Random.int 0 (List.length available - 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseUpCell cell ->
            let
                mode =
                    if model.mode == Start || model.mode == Play then
                        if cellToCheck.bomb then
                            Lose
                        else if Grid.isCleared grid then
                            Win
                        else
                            Play
                    else
                        model.mode

                cellToCheck =
                    if model.mode == Start then
                        newCell
                    else
                        cell

                newCell =
                    Grid.findCellAtPair ( cell.x, cell.y ) newGrid

                newGrid =
                    Grid.updateCell
                        (\cell -> { cell | exposed = True })
                        cell
                        model.grid

                grid =
                    if model.mode == Start then
                        newGrid
                    else
                        Grid.floodCell cell model.grid

                cmd =
                    if model.mode == Start then
                        generateRandomInts model.bombCount grid
                    else
                        Cmd.none
            in
            if model.activeCell == Nothing then
                ( model, Cmd.none )
            else if cell.flagged then
                ( { model | activeCell = Nothing }, Cmd.none )
            else
                ( { model
                    | grid = grid
                    , activeCell = Nothing
                    , mode = mode
                  }
                , cmd
                )

        ArmRandomCells randoms ->
            let
                available =
                    model.grid
                        |> Grid.filter (\c -> not c.bomb && not c.exposed)
                        |> Array.fromList

                cellsToArm : List Cell
                cellsToArm =
                    List.map
                        (\index ->
                            case Array.get index available of
                                Just cell ->
                                    cell

                                Nothing ->
                                    Debug.crash "nah"
                        )
                        randoms

                grid =
                    Grid.updateCells
                        (\c -> { c | bomb = True })
                        cellsToArm
                        model.grid

                exposedCell =
                    grid |> Grid.findCell .exposed

                bombCount =
                    Grid.totalBombs grid
            in
            if bombCount < model.bombCount then
                ( { model | grid = grid }, generateRandomInts (model.bombCount - bombCount) grid )
            else
                ( { model | grid = Grid.floodCell exposedCell grid }, Cmd.none )

        PressDown cell ->
            ( { model | activeCell = Just cell }, Cmd.none )

        ToggleFlag cell ->
            let
                grid =
                    Grid.updateCell (\c -> { c | flagged = not c.flagged }) cell model.grid
            in
            ( { model | grid = grid, activeCell = Nothing }, Cmd.none )

        PressingFace val ->
            ( { model | pressingFace = val }, Cmd.none )

        ClickFace ->
            ( { model | grid = initialGrid, time = 0, mode = Start }, Cmd.none )

        TimeSecond _ ->
            ( { model | time = model.time + 1 }, Cmd.none )


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
            model.grid
                |> Grid.filter .flagged
                |> List.length
    in
    background
        []
        [ frame
            []
            [ viewHeader model.pressingFace hasActiveCell (model.bombCount - flaggedCount) model.time model.mode
            , viewGrid model.activeCell model.grid model.mode
            ]
        ]


viewHeader : Bool -> Bool -> Int -> Int -> GameMode -> Html Msg
viewHeader pressingFace hasActiveCell remainingFlags time mode =
    let
        faceDiv : Element msg
        faceDiv =
            if pressingFace then
                bitmapForFace Pressed
            else if mode == Lose then
                bitmapForFace Sad
            else if mode == Win then
                bitmapForFace Sunglasses
            else if hasActiveCell then
                bitmapForFace Surprised
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
        [ viewDigits
            (if mode == Win then
                0
             else
                remainingFlags
            )
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


viewGrid : Maybe Cell -> Grid -> GameMode -> Html Msg
viewGrid activeCell grid mode =
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
            viewCell size (hasActive activeCell) grid mode

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


viewCell : Int -> Bool -> Grid -> GameMode -> Cell -> Html Msg
viewCell size downOnHover grid mode cell =
    let
        count =
            Grid.neighborBombCount cell grid

        base =
            bitmapForCell count mode cell

        cellDiv =
            styled base
                [ ( "box-sizing", "border-box" )
                , ( "width", px size )
                , ( "height", px size )
                , ( "overflow", "hidden" )
                , ( "cursor", "default" )
                ]

        isPlayable =
            mode == Play || mode == Start

        upDownEvents =
            if isPlayable then
                if cell.flagged then
                    [ onRightClick (ToggleFlag cell) ]
                else
                    [ onMouseUp (MouseUpCell cell)
                    , onMouseDown (PressDown cell)
                    , onRightClick (ToggleFlag cell)
                    ]
            else
                []

        hoverEvents =
            if downOnHover && isPlayable then
                [ onMouseEnter (PressDown cell) ]
            else
                []
    in
    cellDiv
        (List.concat [ upDownEvents, hoverEvents ])
        []


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    onWithOptions "contextmenu"
        { preventDefault = True, stopPropagation = False }
        (Json.succeed message)


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


bitmapForCell : Int -> GameMode -> Cell -> Element msg
bitmapForCell neighbors mode cell =
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
            if cell.exposed then
                if cell.bomb then
                    ( -32, -39 )
                else
                    mapNum neighbors
            else if mode == Lose && cell.bomb then
                if cell.flagged then
                    flag
                else
                    bomb
            else if mode == Lose && not cell.bomb && cell.flagged then
                misflagged
            else if cell.flagged || cell.bomb && mode == Win then
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


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el ([ style css ] ++ attrs) children
