open Xunit
open FsUnit.Xunit

type Direction =
    | R
    | U
    | L
    | D

let spendTime time (map: char array array) =
    let (h, w) = (map.Length, map.[0].Length)

    seq {
        for i in 0 .. (h - 1) do
            for j in 0 .. (w - 1) do
                match map.[i].[j] with
                | '>' ->
                    // '#' を除いて w-2 周期
                    // 端を適当に補正する
                    let nj = (j + time) % (w - 2)
                    let nj = if nj = 0 then w - 2 else nj
                    (i, nj, R)
                | '^' ->
                    let m = h - 2
                    let ni = (m + i - time % m) % m
                    let ni = if ni = 0 then h - 2 else ni
                    (ni, j, U)
                | '<' ->
                    let m = w - 2
                    let nj = (m + j - time % m) % m
                    let nj = if nj = 0 then w - 2 else nj
                    (i, nj, L)
                | 'v' ->
                    let ni = (i + time) % (h - 2)
                    let ni = if ni = 0 then h - 2 else ni
                    (ni, j, D)
                | _ -> ()
    }


let rec bfs (map: char array array) (gi, gj) time (positions: (int * int) Set) =
    if Set.contains (gi, gj) positions then
        time
    else
        assert not (Set.isEmpty positions)
        let (h, w) = (map.Length, map.[0].Length)

        let nextBlizzard = spendTime (time + 1) map

        let set = nextBlizzard |> Seq.map (fun (i, j, _) -> (i, j)) |> Set.ofSeq

        let nextPositions =
            positions
            |> Seq.collect (fun (i, j) ->
                [ (i, j + 1) // R
                  (i - 1, j) // U
                  (i, j - 1) // L
                  (i + 1, j) // D
                  (i, j) ] //
                |> Seq.filter (fun (ni, nj) ->
                    ni >= 0
                    && ni < h
                    && nj >= 0
                    && nj < w
                    && map.[ni].[nj] <> '#'
                    && not (Set.contains (ni, nj) set)))
            |> Set.ofSeq

        bfs map (gi, gj) (time + 1) nextPositions

let solve (map: char array array) (gi, gj) time (si, sj) =
    bfs map (gi, gj) time (Set.singleton ((si, sj)))

let part1 (map: char array array) =
    let (h, w) = (map.Length, map.[0].Length)
    let start = (0, 1)
    let goal = (h - 1, w - 2)

    solve map goal 0 start

let part2 (map: char array array) =
    let (h, w) = (map.Length, map.[0].Length)
    let start = (0, 1)
    let goal = (h - 1, w - 2)

    solve map goal 0 start
    |> fun t -> solve map start t goal
    |> fun t -> solve map goal t start

module Example =
    let map =
        [| // keep format
           "#.######"
           "#>>.<^<#"
           "#.<..<<#"
           "#>v.><>#"
           "#<^v^^>#"
           "######.#" |]
        |> Array.map Seq.toArray

    [<Fact>]
    let testPart1 () = part1 map |> should equal 18

    [<Fact>]
    let testPart2 () = part2 map |> should equal 54

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let map = input |> Array.map Seq.toArray

    part1 map |> printfn "part1: %d"
    part2 map |> printfn "part2: %d"

    0
