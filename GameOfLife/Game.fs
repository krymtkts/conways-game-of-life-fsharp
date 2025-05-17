module GameOfLife.Game

[<Struct>]
type Cell =
    | Dead
    | Live

type Game =
    { height: int
      width: int
      Grid: Cell array }

let inline toXY game index =
    let y = index / game.width
    let x = index % game.width
    x, y

let inline toIndex game x y = y * game.width + x

let inline (|OutOfRange|_|) grid (x, y) =
    x < 0
    || x >= grid.width
    || y < 0
    || y >= grid.height

let inline liveAt grid (x, y) =
    match x, y with
    | OutOfRange grid -> Dead
    | _ ->
        let index = toIndex grid x y
        Array.item index grid.Grid

let inline calcNeighborsRange game index =
    let x, y = toXY game index

    [| x - 1, y + 1
       x, y + 1
       x + 1, y + 1
       x - 1, y
       x + 1, y
       x - 1, y - 1
       x, y - 1
       x + 1, y - 1 |]

let inline countLive cells =
    Array.sumBy
    <| function
        | Live -> 1
        | Dead -> 0
    <| cells

let inline (|Survive|_|) (cell: Cell, lives) = cell.IsLive && (lives = 2 || lives = 3)
let inline (|Birth|_|) (cell: Cell, lives) = cell.IsDead && lives = 3

let inline cycleElement game index element =
    let neighborsLive =
        calcNeighborsRange game index
        |> Array.map (fun neighbor -> liveAt game neighbor)
        |> countLive

    match element, neighborsLive with
    | Birth
    | Survive -> Live
    | _ -> Dead

let inline Cycle game =
    let cycledGrid = Array.mapi (fun x y -> cycleElement game x y) game.Grid
    { game with Grid = cycledGrid }
