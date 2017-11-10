module Main exposing (..)

import Array
import Bitmap as Bitmap exposing (Face(..))
import Element exposing (Element, px, styled)
import GameMode exposing (GameMode(..))
import Grid exposing (Cell, Column, Grid)
import Html exposing (Html, div)
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


type alias Model =
    { grid : Grid
    , activeCell : Maybe Cell
    , pressingFace : Bool
    , game : Game
    , time : Int
    , mode : GameMode
    }


type Game
    = Beginner
    | Intermediate
    | Expert
    | Custom Int Int Int


getBombCount : Game -> Int
getBombCount game =
    case game of
        Beginner ->
            10

        Intermediate ->
            40

        Expert ->
            99

        Custom _ _ count ->
            count


getDimensions : Game -> ( Int, Int )
getDimensions game =
    case game of
        Beginner ->
            ( 9, 9 )

        Intermediate ->
            ( 16, 16 )

        Expert ->
            ( 30, 16 )

        Custom x y _ ->
            ( x, y )


initialGame : Game
initialGame =
    Intermediate


initialModel : Model
initialModel =
    { grid = Grid.fromDimensions (getDimensions initialGame)
    , activeCell = Nothing
    , pressingFace = False
    , game = initialGame
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

        max =
            List.length available - 1
    in
    Random.generate ArmRandomCells <|
        Random.list bombCount <|
            Random.int 0 max


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

                bombCount =
                    getBombCount model.game

                cmd =
                    if model.mode == Start then
                        generateRandomInts bombCount grid
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

                desiredBombCount =
                    getBombCount model.game
            in
            if bombCount < desiredBombCount then
                ( { model | grid = grid }, generateRandomInts (desiredBombCount - bombCount) grid )
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
            ( { model | grid = Grid.fromDimensions (getDimensions model.game), time = 0, mode = Start }, Cmd.none )

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
            [ viewHeader model.pressingFace hasActiveCell (getBombCount model.game - flaggedCount) model.time model.mode
            , viewGrid model.activeCell model.grid model.mode
            ]
        ]


viewHeader : Bool -> Bool -> Int -> Int -> GameMode -> Html Msg
viewHeader pressingFace hasActiveCell remainingFlags time mode =
    let
        faceDiv : Element msg
        faceDiv =
            if pressingFace then
                Bitmap.forFace Pressed
            else if mode == Lose then
                Bitmap.forFace Sad
            else if mode == Win then
                Bitmap.forFace Sunglasses
            else if hasActiveCell then
                Bitmap.forFace Surprised
            else
                Bitmap.forFace Smile

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
                |> List.map (toInt >> Bitmap.forInt >> digit >> (\c -> c [] []))
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
            Bitmap.forCell count mode cell

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
