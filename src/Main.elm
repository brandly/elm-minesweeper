module Main exposing (Model, Msg(..), main, subscriptions, update, view)

import Array exposing (Array)
import Bitmap as Bitmap exposing (Face(..))
import Browser
import Element exposing (Element, px, styled)
import GameMode exposing (GameMode(..))
import Grid exposing (Cell, CellState(..), Grid)
import Html exposing (Html, br, button, div, h1, input, label, p, text)
import Html.Attributes exposing (checked, name, placeholder, style, type_, value)
import Html.Events exposing (custom, onClick, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseUp)
import Json.Decode as Json
import Random
import Time
import Tuple exposing (first, second)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : Grid
    , activeCell : Maybe Cell
    , pressingFace : Bool
    , game : Difficulty
    , time : Int
    , mode : GameMode
    , isRightClicked : Bool
    , menu : Maybe Menu
    }


startGame : Model -> Difficulty -> Model
startGame model difficulty =
    { model
        | game = difficulty
        , grid = Grid.fromDimensions (getDimensions difficulty)
        , time = 0
        , mode = Start
        , menu = Nothing
    }


type Difficulty
    = Beginner
    | Intermediate
    | Expert
    | Custom Int Int Int


type Menu
    = DifficultyMenu
    | CustomDifficultyMenu Int Int Int


getBombCount : Difficulty -> Int
getBombCount difficulty =
    case difficulty of
        Beginner ->
            10

        Intermediate ->
            40

        Expert ->
            99

        Custom _ _ count ->
            count


getDimensions : Difficulty -> ( Int, Int )
getDimensions difficulty =
    case difficulty of
        Beginner ->
            ( 9, 9 )

        Intermediate ->
            ( 16, 16 )

        Expert ->
            ( 30, 16 )

        Custom x y _ ->
            ( x, y )


initialDifficulty : Difficulty
initialDifficulty =
    Intermediate


initialModel : Model
initialModel =
    { grid = Grid.fromDimensions (getDimensions initialDifficulty)
    , activeCell = Nothing
    , pressingFace = False
    , game = initialDifficulty
    , time = 0
    , mode = Start
    , isRightClicked = False
    , menu = Nothing
    }


type Msg
    = MouseUpCell Int Cell
    | MouseDownCell Int Cell
    | RightClick Cell
    | PressingFace Bool
    | ClickFace
    | TimeSecond Time.Posix
    | ArmRandomCells (List Int)
    | ClearActiveCell
    | SetDifficulty Difficulty
    | OpenMenu Menu


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
                exposedBombs : Int
                exposedBombs =
                    List.length <| Grid.filter (\c -> c.state == Exposed && c.bomb) grid

                bothBtnsPressed : Bool
                bothBtnsPressed =
                    model.activeCell /= Nothing && model.isRightClicked

                isSatisfied : Cell -> Bool
                isSatisfied cell_square =
                    Grid.neighborBombCount cell_square model.grid <= Grid.neighborFlagCount cell_square model.grid

                grid : Grid
                grid =
                    if model.mode == Start then
                        Grid.updateCell
                            (\cell_square -> { cell_square | state = Exposed })
                            cell
                            model.grid

                    else if bothBtnsPressed && cell.state == Exposed && isSatisfied cell then
                        Grid.exposeNeighbors cell model.grid

                    else
                        Grid.floodCell cell model.grid

                leftClickResult : ( Model, Cmd Msg )
                leftClickResult =
                    if cell.state == Flagged then
                        ( { model | activeCell = Nothing, isRightClicked = False }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | grid = grid
                            , activeCell = Nothing
                            , mode =
                                if model.mode == Start || model.mode == Play then
                                    if exposedBombs > 0 then
                                        Lose

                                    else if Grid.isCleared grid then
                                        Win

                                    else
                                        Play

                                else
                                    model.mode
                            , isRightClicked = False
                          }
                        , if model.mode == Start then
                            generateRandomInts (getBombCount model.game) grid

                          else
                            Cmd.none
                        )
            in
            if bothBtnsPressed || btn == 1 then
                leftClickResult

            else if btn == 3 then
                ( { model | isRightClicked = False }, Cmd.none )

            else
                ( model, Cmd.none )

        ArmRandomCells randoms ->
            let
                available : Array Cell
                available =
                    model.grid
                        |> Grid.filter (\c -> not c.bomb && c.state /= Exposed)
                        |> Array.fromList

                cellsToArm : List Cell
                cellsToArm =
                    List.filterMap
                        (\index ->
                            Array.get index available
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
                ( { model | grid = grid }
                , generateRandomInts (desiredBombCount - bombCount) grid
                )

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
            ( { model
                | grid = Grid.fromDimensions (getDimensions model.game)
                , time = 0
                , mode = Start
              }
            , Cmd.none
            )

        OpenMenu menu ->
            ( { model | menu = Just menu }
            , Cmd.none
            )

        TimeSecond _ ->
            ( { model | time = model.time + 1 }, Cmd.none )

        ClearActiveCell ->
            ( { model | activeCell = Nothing }, Cmd.none )

        SetDifficulty difficulty ->
            case difficulty of
                _ ->
                    ( { model
                        | game = difficulty
                        , grid = Grid.fromDimensions (getDimensions difficulty)
                        , time = 0
                        , mode = Start
                        , menu = Nothing
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Play ->
            Time.every 1000 TimeSecond

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
                , ( "background-image", "url('./images/windows_xp_bliss-wide.jpg')" )
                , ( "font-family", "Tahoma" )
                ]

        frame =
            styled raisedDiv
                [ ( "display", "inline-block" )
                , ( "background-color", "#bdbdbd" )
                , ( "padding", "5px" )
                ]

        hasActiveCell : Bool
        hasActiveCell =
            isJust model.activeCell

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

        menu =
            case model.menu of
                Just DifficultyMenu ->
                    modalView model

                Just (CustomDifficultyMenu _ _ _) ->
                    modalView model

                Nothing ->
                    Element.none

        toolbarBtn =
            styled button
                [ ( "background", "none" )
                , ( "border", "none" )
                , ( "padding", "0 8px" )
                , ( "outline", "0" )
                , ( "font-size", "14px" )
                ]

        viewToolbar =
            styled div
                [ ( "width", "100%" )
                , ( "height", "24px" )
                , ( "display", "flex" )
                , ( "align-items", "center" )
                ]
    in
    background []
        [ windowsChrome
            [ style "position" "absolute"
            , style "top" "48px"
            , style "left" "96px"
            ]
            [ viewToolbar []
                [ toolbarBtn [ onClick (OpenMenu DifficultyMenu) ] [ text "Set Difficulty" ]
                ]
            , frame []
                [ viewHeader model.pressingFace
                    hasActiveCell
                    (getBombCount model.game - flaggedCount)
                    model.time
                    model.mode
                , viewGrid model.activeCell model.mode unexposedNeighbors model.grid
                ]
            ]
        , menu
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
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "width" "26px"
            , style "height" "26px"
            , style "cursor" "default"
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

        minLen i string =
            if String.length string < i then
                minLen i ("0" ++ string)

            else
                string

        str =
            minLen 3 (String.fromInt n)

        children =
            String.toList str
                |> List.map (Bitmap.forChar >> digit >> (\c -> c [] []))
    in
    frame
        [ style "height" "23px"
        , style "border" "1px solid"
        , style "border-color" "#808080"
        , style "#fff" "#808080"
        ]
        children


viewGrid : Maybe Cell -> GameMode -> List Cell -> Grid -> Html Msg
viewGrid activeCell mode unexposedNeighbors grid =
    let
        size : Int
        size =
            16

        gridWidth : Int
        gridWidth =
            size * List.length grid

        columnHeight : Int
        columnHeight =
            List.head grid
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        gridHeight : Int
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

        renderCell : Cell -> Html Msg
        renderCell =
            viewCell size (isJust activeCell) grid mode

        viewColumn column =
            div
                [ style "display" "inline-block"
                ]
                (column |> List.map (markActive >> renderCell))
    in
    insetDiv
        [ style "width" (px gridWidth)
        , style "height" (px gridHeight)
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
    custom "contextmenu"
        (Json.succeed { message = message, preventDefault = True, stopPropagation = False })


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


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


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


modalView : Model -> Html Msg
modalView model =
    div maskStyle
        [ modalContent []
            [ windowsChrome [ style "padding" "0 18px 90px" ]
                [ formGroup "Difficulty"
                    [ radiobutton "Beginner" Beginner model.game
                    , radiobutton "Intermediate" Intermediate model.game
                    , radiobutton "Expert" Expert model.game
                    , customRadioButton model.menu
                    ]
                ]
            ]
        ]


testDisplay : Maybe Menu -> Html Msg
testDisplay menu =
    label [ style "display" "flex", style "align-items" "center" ]
        [ input
            [ type_ "radio"
            , name "value"
            , onClick (OpenMenu (CustomDifficultyMenu 50 50 50))
            , checked True
            , style "margin" "4px 8px"
            ]
            []
        , text "Custom Difficulty"
        ]


radiobutton : String -> Difficulty -> Difficulty -> Html Msg
radiobutton settingLabel difficulty currentGameDifficulty =
    let
        bombCount =
            getBombCount difficulty

        x =
            first (getDimensions difficulty)

        y =
            second (getDimensions difficulty)
    in
    label [ style "display" "flex", style "align-items" "center" ]
        [ input
            [ type_ "radio"
            , name "value"
            , onClick (SetDifficulty difficulty)
            , checked (difficulty == currentGameDifficulty)
            , style "margin" "4px 8px"
            ]
            []
        , text settingLabel
        ]


customRadioButton : Maybe Menu -> Html Msg
customRadioButton currentMenu =
    let
        isCustomDifficultyMenu menu2 =
            case menu2 of
                CustomDifficultyMenu _ _ _ ->
                    True

                _ ->
                    False
    in
    case currentMenu of
        Nothing ->
            div [] []

        Just menu ->
            case menu of
                DifficultyMenu ->
                    label [ style "display" "flex", style "align-items" "center" ]
                        [ input
                            [ type_ "radio"
                            , name "value"
                            , onClick (OpenMenu (CustomDifficultyMenu 50 50 50))
                            , checked (isCustomDifficultyMenu menu)
                            , style "margin" "4px 8px"
                            ]
                            []
                        , text "Custom Difficulty"
                        ]

                CustomDifficultyMenu x y z ->
                    let
                        handleInput inputString =
                            case String.toInt inputString of
                                Nothing ->
                                    0

                                Just int ->
                                    int
                    in
                    label [ style "display" "flex", style "align-items" "center" ]
                        [ input
                            [ type_ "radio"
                            , name "value"
                            , onClick (OpenMenu (CustomDifficultyMenu x y z))
                            , checked (isCustomDifficultyMenu menu)
                            , style "margin" "4px 8px"
                            ]
                            []
                        , text "Custom Difficulty"
                        , text "Width"
                        , input
                            [ type_ "text"
                            , value (String.fromInt x)
                            , onInput (\inputString -> OpenMenu (CustomDifficultyMenu (handleInput inputString) y z))
                            , style "margin" "4px 8px"
                            ]
                            []
                        , br [] []
                        , text "Height"
                        , input
                            [ type_ "text"
                            , value (String.fromInt y)
                            , onInput (\inputString -> OpenMenu (CustomDifficultyMenu x (handleInput inputString) z))
                            , style "margin" "4px 8px"
                            ]
                            []
                        , br [] []
                        , text "Bomb Count"
                        , input
                            [ type_ "text"
                            , value (String.fromInt z)
                            , onInput (\inputString -> OpenMenu (CustomDifficultyMenu x y (handleInput inputString)))
                            , style "margin" "4px 8px"
                            ]
                            []
                        ]


formGroup : String -> List (Html msg) -> Html msg
formGroup title children =
    div []
        (p [] [ text title ] :: children)


maskStyle : List (Html.Attribute msg)
maskStyle =
    [ style "background-color" "rgba(0,0,0,0.3)"
    , style "position" "fixed"
    , style "top" "0"
    , style "left" "0"
    , style "width" "100%"
    , style "height" "100%"
    ]


modalContent : Element msg
modalContent =
    styled div
        [ ( "position", "absolute" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "height", "auto" )
        , ( "max-height", "80%" )
        , ( "width", "400px" )
        , ( "max-width", "95%" )
        , ( "padding", "10px" )
        , ( "border-radius", "3px" )
        , ( "transform", "translate(-50%, -50%)" )
        ]


windowsChrome : Element msg
windowsChrome =
    styled div
        [ ( "border", "3px solid #135ddf" )
        , ( "background", "#ece9d8" )
        ]
