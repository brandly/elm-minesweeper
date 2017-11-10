module Grid exposing (..)


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


fromDimensions : ( Int, Int ) -> Grid
fromDimensions ( width, height ) =
    let
        makeColumn : Int -> Column
        makeColumn x =
            List.range 1 height
                |> List.map (\y -> Cell x y False False False False)
    in
    List.range 1 width
        |> List.map makeColumn


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


gridToCells : Grid -> List Cell
gridToCells grid =
    List.concat grid


filter : (Cell -> Bool) -> Grid -> List Cell
filter filter grid =
    gridToCells grid |> List.filter filter


findCell : (Cell -> Bool) -> Grid -> Cell
findCell match grid =
    let
        defaultCell =
            Cell -1 -1 False False False False
    in
    gridToCells grid |> findMatching defaultCell match


findCellAtPair : ( Int, Int ) -> Grid -> Cell
findCellAtPair ( x, y ) grid =
    grid |> findCell (\cell -> cell.x == x && cell.y == y)


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


updateCells : (Cell -> Cell) -> List Cell -> Grid -> Grid
updateCells update cells grid =
    let
        head =
            case List.head cells of
                Just cell ->
                    cell

                Nothing ->
                    Cell -1 -1 False False False False

        tail =
            case List.tail cells of
                Just cells ->
                    cells

                Nothing ->
                    []

        newGrid =
            updateCell update head grid
    in
    if List.length cells > 0 then
        updateCells update tail newGrid
    else
        newGrid


updateCell : (Cell -> Cell) -> Cell -> Grid -> Grid
updateCell update cell grid =
    let
        replaceCell : Column -> Column
        replaceCell =
            List.map
                (\og ->
                    if og.x == cell.x && og.y == cell.y then
                        update cell
                    else
                        og
                )
    in
    grid |> List.map replaceCell


neighborBombCount : Cell -> Grid -> Int
neighborBombCount cell grid =
    List.length <| List.filter .bomb <| getNeighbors cell grid


getNeighbors : Cell -> Grid -> List Cell
getNeighbors cell grid =
    grid |> filter (isNeighbor cell)


isNeighbor : Cell -> Cell -> Bool
isNeighbor a b =
    abs (a.x - b.x) <= 1 && abs (a.y - b.y) <= 1


totalBombs : Grid -> Int
totalBombs grid =
    List.length <| filter .bomb grid


isCleared : Grid -> Bool
isCleared grid =
    grid
        |> filter (\c -> not c.bomb && not c.exposed)
        |> List.length
        |> (==) 0


floodCell : Cell -> Grid -> Grid
floodCell cell grid =
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
                    |> List.filter (\c -> not c.bomb && not c.exposed && not c.flagged)
            else
                []

        moreToExpose =
            List.concat [ tail, additional ]
    in
    if List.length moreToExpose > 0 then
        floodCells moreToExpose newGrid
    else
        newGrid


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
