open Xunit
open FsUnit.Xunit

type Obstacle =
    | Tree of int
    | GridEdge of int

let solve (height: int array array) =
    let n, m = height.Length, height.[0].Length

    Seq.allPairs [ 0 .. (n - 1) ] [ 0 .. (m - 1) ]
    |> Seq.map (fun (i, j) ->
        let h = height.[i].[j]

        [
          // U
          [ 0 .. (i - 1) ]
          |> Seq.tryFindBack (fun i' -> h <= height.[i'].[j])
          |> Option.map (fun i' -> Tree(i - i'))
          |> Option.defaultValue (GridEdge i)

          // L
          [ 0 .. (j - 1) ]
          |> Seq.tryFindBack (fun j' -> h <= height.[i].[j'])
          |> Option.map (fun j' -> Tree(j - j'))
          |> Option.defaultValue (GridEdge j)

          // D
          [ (i + 1) .. (n - 1) ]
          |> Seq.tryFind (fun i' -> h <= height.[i'].[j])
          |> Option.map (fun i' -> Tree(i' - i))
          |> Option.defaultValue (GridEdge(n - i - 1))

          // R
          [ (j + 1) .. (m - 1) ]
          |> Seq.tryFind (fun j' -> h <= height.[i].[j'])
          |> Option.map (fun j' -> Tree(j' - j))
          |> Option.defaultValue (GridEdge(m - j - 1)) ])

let part1 (height: int array array) =
    solve height
    |> Seq.filter (fun obstacles ->
        obstacles
        |> Seq.exists (function
            | Tree _ -> false
            | GridEdge _ -> true))
    |> Seq.length

let part2 (height: int array array) =
    solve height
    |> Seq.map (fun obstacles ->
        obstacles
        |> Seq.map (function
            | Tree n -> n
            | GridEdge n -> n)
        |> Seq.reduce (*))
    |> Seq.max

let parse (input: string seq) =
    input
    |> Seq.map (fun s -> s |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq)
    |> Array.ofSeq

module Example =
    let input =
        [ "30373" // keep format
          "25512"
          "65332"
          "33549"
          "35390" ]

    let height = parse input

    [<Fact>]
    let testPart1 () = part1 height |> should equal 21

    let testPart2 () = part2 height |> should equal 8


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let height = parse input

    part1 height |> printfn "part1: %d"
    part2 height |> printfn "part2: %d"

    0
