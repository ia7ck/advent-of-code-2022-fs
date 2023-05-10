open Xunit
open FsUnit.Xunit

type Instruction =
    | AddX of int
    | NoOp

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        if s.StartsWith("addx ") then
            let v = s.Replace("addx ", "") |> int
            AddX v
        else if s = "noop" then
            NoOp
        else
            failwith $"s = {s}")

let execute instructions =
    ([ 1 ], instructions)
    ||> Seq.fold (fun xs instruction ->
        let x = List.head xs

        match instruction with
        | AddX v -> x + v :: x :: xs
        | NoOp -> x :: xs)
    |> Seq.tail // 最後の X は不要
    |> Seq.rev

let signalStrength history =
    [ 20; 60; 100; 140; 180; 220 ]
    |> Seq.map (fun i ->
        let x = Seq.item (i - 1) history
        i * x)
    |> Seq.sum

let part1 instructions = execute instructions |> signalStrength

let part2 instructions =
    execute instructions
    |> Seq.chunkBySize 40
    |> Seq.map (fun xs ->
        xs
        |> Seq.indexed
        |> Seq.map (fun (i, x) -> if x - 1 <= i && i <= x + 1 then "#" else ".")
        |> String.concat "")

module Example =
    let input =
        [ "addx 15"
          "addx -11"
          "addx 6"
          "addx -3"
          "addx 5"
          "addx -1"
          "addx -8"
          "addx 13"
          "addx 4"
          "noop"
          "addx -1"
          "addx 5"
          "addx -1"
          "addx 5"
          "addx -1"
          "addx 5"
          "addx -1"
          "addx 5"
          "addx -1"
          "addx -35"
          "addx 1"
          "addx 24"
          "addx -19"
          "addx 1"
          "addx 16"
          "addx -11"
          "noop"
          "noop"
          "addx 21"
          "addx -15"
          "noop"
          "noop"
          "addx -3"
          "addx 9"
          "addx 1"
          "addx -3"
          "addx 8"
          "addx 1"
          "addx 5"
          "noop"
          "noop"
          "noop"
          "noop"
          "noop"
          "addx -36"
          "noop"
          "addx 1"
          "addx 7"
          "noop"
          "noop"
          "noop"
          "addx 2"
          "addx 6"
          "noop"
          "noop"
          "noop"
          "noop"
          "noop"
          "addx 1"
          "noop"
          "noop"
          "addx 7"
          "addx 1"
          "noop"
          "addx -13"
          "addx 13"
          "addx 7"
          "noop"
          "addx 1"
          "addx -33"
          "noop"
          "noop"
          "noop"
          "addx 2"
          "noop"
          "noop"
          "noop"
          "addx 8"
          "noop"
          "addx -1"
          "addx 2"
          "addx 1"
          "noop"
          "addx 17"
          "addx -9"
          "addx 1"
          "addx 1"
          "addx -3"
          "addx 11"
          "noop"
          "noop"
          "addx 1"
          "noop"
          "addx 1"
          "noop"
          "noop"
          "addx -13"
          "addx -19"
          "addx 1"
          "addx 3"
          "addx 26"
          "addx -30"
          "addx 12"
          "addx -1"
          "addx 3"
          "addx 1"
          "noop"
          "noop"
          "noop"
          "addx -9"
          "addx 18"
          "addx 1"
          "addx 2"
          "noop"
          "noop"
          "addx 9"
          "noop"
          "noop"
          "noop"
          "addx -1"
          "addx 2"
          "addx -37"
          "addx 1"
          "addx 3"
          "noop"
          "addx 15"
          "addx -21"
          "addx 22"
          "addx -6"
          "addx 1"
          "noop"
          "addx 2"
          "addx 1"
          "noop"
          "addx -10"
          "noop"
          "noop"
          "addx 20"
          "addx 1"
          "addx 2"
          "addx 2"
          "addx -6"
          "addx -11"
          "noop"
          "noop"
          "noop" ]

    let instructions = parse input

    [<Fact>]
    let testPart1 () =
        part1 instructions |> should equal 13140

    [<Fact>]
    let testPart2 () =
        part2 instructions
        |> List.ofSeq
        |> should
            equal
            [ "##..##..##..##..##..##..##..##..##..##.."
              "###...###...###...###...###...###...###."
              "####....####....####....####....####...."
              "#####.....#####.....#####.....#####....."
              "######......######......######......####"
              "#######.......#######.......#######....." ]

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let instructions = parse input

    part1 instructions |> printfn "part1: %d"
    part2 instructions |> String.concat "\n" |> printfn "part2:\n%s"

    0
