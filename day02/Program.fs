open Xunit
open FsUnit.Xunit

type Shape =
    | Rock
    | Paper
    | Scissors

    member this.Score =
        match this with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type Outcome =
    | Lose
    | Draw
    | Win

    member this.Score =
        match this with
        | Lose -> 0
        | Draw -> 3
        | Win -> 6

let judge you opponent =
    match you, opponent with
    | Rock, Rock -> Draw
    | Rock, Paper -> Lose
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Paper, Paper -> Draw
    | Paper, Scissors -> Lose
    | Scissors, Rock -> Lose
    | Scissors, Paper -> Win
    | Scissors, Scissors -> Draw

let abc input =
    match input with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwith $"input = {input}"

let part1 guide =
    let xyz input =
        match input with
        | 'X' -> Rock
        | 'Y' -> Paper
        | 'Z' -> Scissors
        | _ -> failwith $"input = {input}"

    guide
    |> Seq.sumBy (fun (opponent, you) ->
        let opponent, you = abc opponent, xyz you
        let outcome = judge you opponent
        you.Score + outcome.Score)

let part2 guide =
    let xyz input =
        match input with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith $"input = {input}"

    guide
    |> Seq.sumBy (fun (opponent, outcome) ->
        let opponent, outcome = abc opponent, xyz outcome

        let you =
            [ Rock; Paper; Scissors ] |> List.find (fun y -> judge y opponent = outcome)

        you.Score + outcome.Score)

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        match s.Split(" ") with
        | [| l; r |] -> char l, char r
        | _ -> failwith $"s = {s}")

module Example =
    let input = [ "A Y"; "B X"; "C Z" ]
    let guide = parse input

    [<Fact>]
    let testPart1 () = part1 guide |> should equal 15

    [<Fact>]
    let testPart2 () = part2 guide |> should equal 12

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let guide = parse input

    part1 guide |> printfn "part1: %d"
    part2 guide |> printfn "part2: %d"

    0
