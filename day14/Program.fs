open Xunit
open FsUnit.Xunit

type Cave =
    | Abyss of int // 砂の y がこの値になると無限に落ちる
    | Floor of int // 砂の y はこの値未満でないといけない

let rec down (x, y) cave rocks sands =
    [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
    |> Seq.tryFind (fun (nx, ny) ->
        let ylim =
            match cave with
            | Abyss y' -> y'
            | Floor y' -> y' - 1

        ny <= ylim
        && not (Set.contains (nx, ny) rocks)
        && not (Set.contains (nx, ny) sands))
    |> Option.map (fun (nx, ny) ->
        match cave with
        | Abyss y' when ny = y' -> sands // 砂が無限に落ち続けるのでシミュレーションを打ち切る
        | _ -> down (nx, ny) cave rocks sands)
    |> Option.defaultWith (fun () -> Set.add (x, y) sands)

let countSand cave rocks =
    let rec loop sands =
        let sands' = down (500, 0) cave rocks sands
        let count = Set.count sands'

        if count = Set.count sands then count else loop sands'

    loop Set.empty

let part1 rocks =
    let ymax = rocks |> Seq.map snd |> Seq.max

    countSand (Abyss ymax) rocks

let part2 rocks =
    let ymax = rocks |> Seq.map snd |> Seq.max

    countSand (Floor(ymax + 2)) rocks

let parse (input: string seq) =
    input
    |> Seq.collect (fun s ->
        let points =
            s.Split("->")
            |> Seq.map (fun xy ->
                match xy.Split(",") with
                | [| x; y |] -> (int x, int y)
                | _ -> failwith $"xy = {xy}") in

        points
        |> Seq.zip (Seq.skip 1 points)
        |> Seq.collect (fun ((x, y), (xx, yy)) ->
            if x = xx then
                [ min y yy .. max y yy ] |> Seq.map (fun y' -> (x, y'))
            else if y = yy then
                [ min x xx .. max x xx ] |> Seq.map (fun x' -> (x', y))
            else
                failwith $"x = {x}, y = {y}, xx = {xx}, yy = {yy}"))
    |> Set.ofSeq

module Example =
    let input = [ "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9" ]
    let rocks = parse input

    [<Fact>]
    let testPart1 () = part1 rocks |> should equal 24

    [<Fact>]
    let testPart2 () = part2 rocks |> should equal 93

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let rocks = parse input

    part1 rocks |> printfn "part1: %d"
    part2 rocks |> printfn "part2: %d"

    0
