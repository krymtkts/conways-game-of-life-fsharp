open System
open GameOfLife.Game
open System.IO

let writer =
    let sw = new StreamWriter(Console.OpenStandardOutput())
    sw.AutoFlush <- false
    sw |> Console.SetOut
    sw

let inline AliveCount grid =
    List.sumBy (fun x -> if x = Live then 1 else 0) grid

let inline printGameState game loop =
    let symbol cell =
        match cell with
        | Live -> "X"
        | Dead -> " "

    let alive = AliveCount game.Grid

    $"#Loop: {loop}        Living: {alive}"
    |> Console.WriteLine

    "" |> Console.WriteLine

    List.iteri
        (fun index cell ->
            match index % game.width with
            | a when a + 1 = game.width -> symbol cell |> Console.WriteLine
            | _ -> symbol cell |> Console.Write)
        game.Grid

    writer.Flush()

let rec GameLoop game loopNumber =
    do Console.Clear()
    do printGameState game loopNumber
    do Async.Sleep 100 |> Async.RunSynchronously

    match AliveCount game.Grid with
    | 0 -> "Game Over" |> Console.Write
    | _ -> game |> Cycle |> GameLoop <| 1 + loopNumber

let MakeGameBoard height width =
    let random = Random()

    List.init (height * width) (fun index ->
        if random.NextDouble() > 0.41 then
            Live
        else
            Dead)

[<EntryPoint>]
let main argv =
    $"Inputs: {argv}" |> Console.WriteLine
    let height = Array.get argv 0 |> Int32.Parse
    let width = Array.get argv 1 |> Int32.Parse

    let initialGameState =
        { Grid = MakeGameBoard height width
          height = height
          width = width }

    GameLoop initialGameState 0
    0 // return an integer exit code
