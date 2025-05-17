open System
open GameOfLife.Game
open System.IO

let writer =
    let sw = new StreamWriter(Console.OpenStandardOutput())
    sw.AutoFlush <- false
    sw |> Console.SetOut
    sw

let inline printGameState game loop alive =
    let symbol cell =
        match cell with
        | Live -> "X"
        | Dead -> " "

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
    let alive = countLive game.Grid

    do Console.Clear()
    do printGameState game loopNumber alive
    do Async.Sleep 100 |> Async.RunSynchronously

    match alive with
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
