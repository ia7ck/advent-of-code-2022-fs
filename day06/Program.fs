open Xunit
open FsUnit.Xunit

let solve s w =
    s
    |> Seq.windowed w
    |> Seq.indexed
    |> Seq.tryFind (fun (_, t) -> t = Array.distinct t)
    |> Option.map (fun (i, _) -> i + w)

let part1 s = solve s 4 |> Option.get

let part2 s = solve s 14 |> Option.get

module Example =
    let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

    [<Fact>]
    let testPart1 () = part1 input |> should equal 7

    [<Fact>]
    let testPart2 () = part2 input |> should equal 19

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()

    part1 input |> printfn "part1: %d"
    part2 input |> printfn "part2: %d"

    0
