open Xunit
open FsUnit.Xunit

let rec bfs (end_i, end_j) (height: int array array) points (visited: (int * int) Set) step =
    if Seq.isEmpty points then
        None
    else if Seq.contains (end_i, end_j) points then
        Some step
    else
        let nextPoints =
            points
            |> Seq.collect (fun (i, j) ->
                [ (i - 1, j); (i, j - 1); (i + 1, j); (i, j + 1) ]
                |> Seq.filter (fun (ni, nj) ->
                    0 <= ni
                    && ni < height.Length
                    && 0 <= nj
                    && nj < height.[ni].Length
                    && not (Set.contains (ni, nj) visited)
                    && height.[ni].[nj] <= height.[i].[j] + 1))
            |> Seq.distinct

        let nextVisited =
            (visited, nextPoints) ||> Seq.fold (fun acc (ni, nj) -> Set.add (ni, nj) acc)

        bfs (end_i, end_j) height nextPoints nextVisited (step + 1)

let part1 start end_ height =
    bfs end_ height [ start ] Set.empty 0 |> Option.get

let part2 end_ (height: int array array) =
    let starts =
        Seq.allPairs [ 0 .. (height.Length - 1) ] [ 0 .. (height.[0].Length - 1) ]
        |> Seq.filter (fun (i, j) -> height.[i].[j] = 0)

    bfs end_ height starts Set.empty 0 |> Option.get

let parse (input: string seq) =
    let height =
        input
        |> Seq.map (fun s ->
            s
            |> Seq.map (fun ch ->
                match ch with
                | 'S' -> 0
                | 'E' -> 25
                | ch when 'a' <= ch && ch <= 'z' -> int ch - int 'a'
                | _ -> failwith $"ch = {ch}")
            |> Array.ofSeq)
        |> Array.ofSeq

    let n, m = height.Length, height.[0].Length

    let find ch =
        Seq.allPairs [ 0 .. (n - 1) ] [ 0 .. (m - 1) ]
        |> Seq.find (fun (i, j) -> input |> Seq.item i |> Seq.item j |> ((=) ch))

    let start = find 'S'
    let end_ = find 'E'

    start, end_, height

module Example =
    let input =
        [ "Sabqponm" // keep format
          "abcryxxl"
          "accszExk"
          "acctuvwj"
          "abdefghi" ]

    let start, end_, height = parse input

    [<Fact>]
    let testPart1 () =
        part1 start end_ height |> should equal 31

    [<Fact>]
    let testPart2 () = part2 end_ height |> should equal 29

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let start, end_, height = parse input

    part1 start end_ height |> printfn "part1: %d"
    part2 end_ height |> printfn "part2: %d"

    0
