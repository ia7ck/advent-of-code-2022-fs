open Xunit
open FsUnit.Xunit

let part1 (calories: int seq seq) =
    calories |> Seq.map (Seq.sum) |> Seq.max

let part2 (calories: int seq seq) =
    calories |> Seq.map (Seq.sum) |> Seq.sortDescending |> Seq.take 3 |> Seq.sum

let parse (input: string) =
    input.Split("\n\n")
    |> Seq.map (fun s -> s.Split("\n") |> Seq.map (fun calory -> int calory))

module Example =
    let input =
        "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

    let calories = parse input

    [<Fact>]
    let testPart1 () = part1 calories |> should equal 24000

    [<Fact>]
    let testPart2 () = part2 calories |> should equal 45000

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let calories = parse input

    part1 calories |> printfn "part1: %d"
    part2 calories |> printfn "part2: %d"

    0
