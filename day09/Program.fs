open Xunit
open FsUnit.Xunit

type Dir =
    | R
    | U
    | L
    | D

let sign (x: int) = System.Math.Sign(x)

let followHead knots =
    (List.head knots, List.tail knots)
    ||> List.scan (fun (pi, pj) (i, j) ->
        let di, dj = pi - i, pj - j in

        if max (abs di) (abs dj) <= 1 then
            (i, j)
        else
            (i + sign di, j + sign dj))

let solve motions k =
    let knots = List.init k (fun _ -> 0, 0)

    let moves =
        motions
        |> Seq.collect (fun (dir, n) ->
            let di, dj =
                match dir with
                | R -> 0, 1
                | U -> -1, 0
                | L -> 0, -1
                | D -> 1, 0

            Seq.init n (fun _ -> di, dj))

    ((knots, Set.empty), moves)
    ||> Seq.fold (fun (knots, tailVisited) (di, dj) ->
        let hi, hj = List.head knots
        let knots = followHead ((hi + di, hj + dj) :: List.tail knots)
        knots, Set.add (List.last knots) tailVisited)
    |> snd

let part1 motions = solve motions 2 |> Set.count
let part2 motions = solve motions 10 |> Set.count

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        match s.Split(" ") with
        | [| "R"; n |] -> R, int n
        | [| "U"; n |] -> U, int n
        | [| "L"; n |] -> L, int n
        | [| "D"; n |] -> D, int n
        | _ -> failwith $"s = {s}")

module Example =
    let input = [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ]
    let motions = parse input

    [<Fact>]
    let testPart1 () = part1 motions |> should equal 13

    [<Fact>]
    let testPart2 () = part2 motions |> should equal 1


    [<Fact>]
    let testPart2Large () =
        [ "R 5"; "U 8"; "L 8"; "D 3"; "R 17"; "D 10"; "L 25"; "U 20" ]
        |> parse
        |> part2
        |> should equal 36


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let motions = parse input

    part1 motions |> printfn "part1: %d"
    part2 motions |> printfn "part2: %d"

    0
