module Main exposing (Model, Msg(..), main, subscriptions, update, view)

import Array exposing (Array)
import Bitmap as Bitmap exposing (Face(..))
import Browser
import Element exposing (Element, px, styled)
import GameMode exposing (GameMode(..))
import Grid exposing (Cell, CellState(..), Grid)
import Html exposing (Html, button, div, input, label, p, text)
import Html.Attributes exposing (checked, disabled, name, style, type_, value)
import Html.Events exposing (custom, onClick, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseUp)
import Json.Decode as Json
import Random
import Time


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
    , difficulty : Difficulty
    , time : Int
    , mode : GameMode
    , isRightClicked : Bool
    , menu : Maybe Menu
    }


startGame : Difficulty -> Model
startGame difficulty =
    { initialModel
        | difficulty = difficulty
        , grid = Grid.fromDimensions (getDimensions difficulty)
    }


initialModel : Model
initialModel =
    { grid = Grid.fromDimensions (getDimensions initialDifficulty)
    , activeCell = Nothing
    , pressingFace = False
    , difficulty = initialDifficulty
    , time = 0
    , mode = Start
    , isRightClicked = False
    , menu = Nothing
    }


type Difficulty
    = Beginner
    | Intermediate
    | Expert
    | Custom GridProperties


type alias GridProperties =
    { width : Int
    , height : Int
    , bombs : Int
    }


isCustom : Difficulty -> Bool
isCustom difficulty =
    case difficulty of
        Custom _ ->
            True

        _ ->
            False


getBombCount : Difficulty -> Int
getBombCount difficulty =
    case difficulty of
        Beginner ->
            10

        Intermediate ->
            40

        Expert ->
            99

        Custom { bombs } ->
            bombs


getDimensions : Difficulty -> ( Int, Int )
getDimensions difficulty =
    case difficulty of
        Beginner ->
            ( 9, 9 )

        Intermediate ->
            ( 16, 16 )

        Expert ->
            ( 30, 16 )

        Custom { width, height } ->
            ( width, height )


initialDifficulty : Difficulty
initialDifficulty =
    Intermediate


type Msg
    = MouseUpCell Int Cell
    | MouseDownCell Int Cell
    | RightClick Cell
    | PressingFace Bool
    | ClickFace
    | TimeSecond Time.Posix
    | ArmRandomCells (List Int)
    | ClearActiveCell
    | OpenMenu Difficulty
    | MenuMsg MenuMsg
    | CloseMenu (Maybe Difficulty)


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
                            generateRandomInts (getBombCount model.difficulty) grid

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
                    getBombCount model.difficulty
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
                | grid = Grid.fromDimensions (getDimensions model.difficulty)
                , time = 0
                , mode = Start
              }
            , Cmd.none
            )

        TimeSecond _ ->
            ( { model | time = model.time + 1 }, Cmd.none )

        ClearActiveCell ->
            ( { model | activeCell = Nothing }, Cmd.none )

        OpenMenu difficulty ->
            let
                fields =
                    case difficulty of
                        Custom properties ->
                            properties

                        _ ->
                            { width = 5, height = 5, bombs = 5 }
            in
            ( { model | menu = Just (DifficultyMenu difficulty fields) }, Cmd.none )

        MenuMsg msg_ ->
            ( { model | menu = Maybe.map (updateMenu msg_) model.menu }, Cmd.none )

        CloseMenu (Just difficulty) ->
            ( startGame difficulty, Cmd.none )

        CloseMenu Nothing ->
            ( { model | menu = Nothing }, Cmd.none )


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
                        Grid.getNeighbors cell model.grid
                            |> List.filter (\c -> c.state == Initial)

                    else
                        []

                Nothing ->
                    []

        menu =
            case model.menu of
                Just customMenu ->
                    viewModal customMenu

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
                [ toolbarBtn [ onClick (OpenMenu model.difficulty) ]
                    [ text "Set Difficulty" ]
                ]
            , frame []
                [ viewHeader model.pressingFace
                    hasActiveCell
                    (getBombCount model.difficulty - flaggedCount)
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


type Menu
    = DifficultyMenu Difficulty GridProperties


type MenuMsg
    = SetDifficulty Difficulty


updateMenu : MenuMsg -> Menu -> Menu
updateMenu msg (DifficultyMenu _ fields) =
    case msg of
        SetDifficulty (Custom properties) ->
            DifficultyMenu (Custom properties) properties

        SetDifficulty d ->
            DifficultyMenu d fields


viewModal : Menu -> Html Msg
viewModal (DifficultyMenu difficulty fields) =
    let
        menuContent =
            Html.map MenuMsg <|
                div []
                    [ radiobutton "Beginner" (difficulty == Beginner) Beginner
                    , radiobutton "Intermediate" (difficulty == Intermediate) Intermediate
                    , radiobutton "Expert" (difficulty == Expert) Expert
                    , radiobutton "Custom" (isCustom difficulty) (Custom fields)
                    , viewCustomFields (isCustom difficulty) fields
                    ]
    in
    div maskStyle
        [ modalContent []
            [ windowsChrome [ style "padding" "0 18px 90px" ]
                [ formGroup "Difficulty"
                    [ menuContent
                    , button [ onClick (CloseMenu (Just difficulty)) ] [ text "OK" ]
                    , button [ onClick (CloseMenu Nothing) ] [ text "Cancel" ]
                    ]
                ]
            ]
        ]


radiobutton : String -> Bool -> Difficulty -> Html MenuMsg
radiobutton settingLabel isSelected difficulty =
    label [ style "display" "flex", style "align-items" "center" ]
        [ input
            [ type_ "radio"
            , name "value"
            , onClick (SetDifficulty difficulty)
            , checked isSelected
            , style "margin" "4px 8px"
            ]
            []
        , text settingLabel
        ]


viewCustomFields : Bool -> GridProperties -> Html MenuMsg
viewCustomFields enabled ({ width, height, bombs } as fields) =
    let
        toInput num onInput_ =
            input
                [ type_ "number"
                , value (String.fromInt num)
                , onInput
                    (String.toInt
                        >> Maybe.withDefault 0
                        >> onInput_
                    )
                , style "margin" "4px 8px"
                , disabled (not enabled)
                ]
                []

        clampBombs num =
            if num > width * height - 1 then
                abs (width * height - 1)

            else
                abs num
    in
    div []
        [ toInput width
            (\num ->
                SetDifficulty (Custom { fields | width = clamp 1 20 num })
            )
        , toInput height
            (\num ->
                SetDifficulty (Custom { fields | height = clamp 1 20 num })
            )
        , toInput bombs
            (\num ->
                SetDifficulty (Custom { fields | bombs = clampBombs num })
            )
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
