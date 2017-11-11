module Main exposing (..)

import Array
import Bitmap as Bitmap exposing (Face(..))
import Element exposing (Element, px, styled)
import GameMode exposing (GameMode(..))
import Grid exposing (Cell, CellState(Exposed, Flagged, Initial), Column, Grid)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseUp, onWithOptions)
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
    , isRightClicked : Bool
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
    , isRightClicked = False
    }


type Msg
    = MouseUpCell Int Cell
    | MouseDownCell Int Cell
    | RightClick Cell
    | PressingFace Bool
    | ClickFace
    | TimeSecond Time
    | ArmRandomCells (List Int)
    | ClearActiveCell


generateRandomInts : Int -> Grid -> Cmd Msg
generateRandomInts bombCount grid =
    let
        available =
            grid |> Grid.filter (\c -> not c.bomb && c.state /= Exposed)

        max =
            List.length available - 1
    in
    Random.generate ArmRandomCells <|
        Random.list bombCount <|
            Random.int 0 max


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseUpCell btn cell ->
            let
                exposedBombs =
                    List.length <| Grid.filter (\c -> c.state == Exposed && c.bomb) grid

                mode =
                    if model.mode == Start || model.mode == Play then
                        if exposedBombs > 0 then
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
                        (\cell -> { cell | state = Exposed })
                        cell
                        model.grid

                bothBtnsPressed =
                    model.activeCell /= Nothing && model.isRightClicked

                isSatisfied cell =
                    Grid.neighborBombCount cell model.grid <= Grid.neighborFlagCount cell model.grid

                grid =
                    if model.mode == Start then
                        newGrid
                    else if bothBtnsPressed && cell.state == Exposed && isSatisfied cell then
                        Grid.exposeNeighbors cell model.grid
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
            if bothBtnsPressed || btn == 1 then
                ( { model
                    | grid = grid
                    , activeCell = Nothing
                    , mode = mode
                    , isRightClicked = False
                  }
                , cmd
                )
            else if btn == 3 then
                ( { model | isRightClicked = False }, Cmd.none )
            else
                ( model, Cmd.none )

        ArmRandomCells randoms ->
            let
                available =
                    model.grid
                        |> Grid.filter (\c -> not c.bomb && c.state /= Exposed)
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
                    grid |> Grid.findCell (\c -> c.state == Exposed)

                bombCount =
                    Grid.totalBombs grid

                desiredBombCount =
                    getBombCount model.game
            in
            if bombCount < desiredBombCount then
                ( { model | grid = grid }, generateRandomInts (desiredBombCount - bombCount) grid )
            else
                ( { model | grid = Grid.floodCell exposedCell grid }, Cmd.none )

        MouseDownCell btn cell ->
            let
                model_ =
                    if btn == 1 then
                        { model | activeCell = Just cell }
                    else
                        model
            in
            ( model_, Cmd.none )

        RightClick cell ->
            let
                grid =
                    Grid.toggleFlag cell model.grid
            in
            ( { model | grid = grid, isRightClicked = True }, Cmd.none )

        PressingFace val ->
            ( { model | pressingFace = val }, Cmd.none )

        ClickFace ->
            ( { model | grid = Grid.fromDimensions (getDimensions model.game), time = 0, mode = Start }, Cmd.none )

        TimeSecond _ ->
            ( { model | time = model.time + 1 }, Cmd.none )

        ClearActiveCell ->
            ( { model | activeCell = Nothing }, Cmd.none )


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
                |> Grid.filter (\c -> c.state == Flagged)
                |> List.length

        unexposedNeighbors =
            case model.activeCell of
                Just cell ->
                    if model.isRightClicked && cell.state == Exposed then
                        Grid.getNeighbors cell model.grid |> List.filter (\c -> c.state == Initial)
                    else
                        []

                Nothing ->
                    []
    in
    background
        []
        [ frame
            []
            [ viewHeader model.pressingFace hasActiveCell (getBombCount model.game - flaggedCount) model.time model.mode
            , viewGrid model.activeCell model.mode unexposedNeighbors model.grid
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


viewGrid : Maybe Cell -> GameMode -> List Cell -> Grid -> Html Msg
viewGrid activeCell mode unexposedNeighbors grid =
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
            if List.member cell unexposedNeighbors then
                { cell | active = True }
            else
                case activeCell of
                    Just active ->
                        if active == cell && cell.state == Initial then
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
        , onMouseLeave ClearActiveCell
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
            [ onWhichMouseUp (\btn -> MouseUpCell btn cell)
            , onWhichMouseDown (\btn -> MouseDownCell btn cell)
            , onRightClick (RightClick cell)
            ]

        hoverEvents =
            if downOnHover then
                [ onMouseEnter (MouseDownCell 1 cell) ]
            else
                []
    in
    cellDiv
        (if isPlayable then
            List.concat [ upDownEvents, hoverEvents ]
         else
            []
        )
        []


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    onWithOptions "contextmenu"
        { preventDefault = True, stopPropagation = False }
        (Json.succeed message)


buildWhich : String -> (Int -> msg) -> Html.Attribute msg
buildWhich event toMsg =
    Html.Events.on event
        (Json.map toMsg (Json.at [ "which" ] Json.int))


onWhichMouseUp : (Int -> msg) -> Html.Attribute msg
onWhichMouseUp =
    buildWhich "mouseup"


onWhichMouseDown : (Int -> msg) -> Html.Attribute msg
onWhichMouseDown =
    buildWhich "mousedown"


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
