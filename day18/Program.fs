open Xunit
open FsUnit.Xunit

let adjacents (x, y, z) =
    [ (x - 1, y, z)
      (x + 1, y, z)
      (x, y - 1, z)
      (x, y + 1, z)
      (x, y, z - 1)
      (x, y, z + 1) ]

let part1 cubes =
    cubes
    |> Seq.sumBy (fun cube -> adjacents cube |> Seq.except cubes |> Seq.length)


let part2 cubes =
    let xmin = cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.min
    let xmax = cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.max
    let ymin = cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.min
    let ymax = cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.max
    let zmin = cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.min
    let zmax = cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.max

    // 入力にこういうのが含まれていると困るけどなさそう
    //
    // .........
    // .#######.
    // .#.....#.
    // .#.###.#.
    // .#.#.#.#.
    // .#.###.#.
    // .#.....#.
    // .#######.
    // .........

    let rec dfs (x, y, z) visited =
        (visited, adjacents (x, y, z))
        ||> Seq.fold (fun visited' (x', y', z') ->
            if
                xmin - 1 <= x'
                && x' <= xmax + 1
                && ymin - 1 <= y'
                && y' <= ymax + 1
                && zmin - 1 <= z'
                && z' <= zmax + 1
                && not (Set.contains (x', y', z') visited')
                && not (Seq.contains (x', y', z') cubes)
            then
                dfs (x', y', z') (Set.add (x', y', z') visited')
            else
                visited')

    // 「外側」の座標を集める
    let start = (xmin - 1, ymin - 1, zmin - 1)
    let visited = dfs start (Set [ start ])

    cubes
    |> Seq.sumBy (fun cube ->
        adjacents cube
        |> Seq.except cubes
        |> Seq.filter (fun c -> Set.contains c visited) // 「外側」に隣接している面のみ数える
        |> Seq.length)

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        match s.Trim().Split(",") with
        | [| x; y; z |] -> int x, int y, int z
        | _ -> failwith $"s = {s}")

module Example =
    let cubes =
        [ (2, 2, 2)
          (1, 2, 2)
          (3, 2, 2)
          (2, 1, 2)
          (2, 3, 2)
          (2, 2, 1)
          (2, 2, 3)
          (2, 2, 4)
          (2, 2, 6)
          (1, 2, 5)
          (3, 2, 5)
          (2, 1, 5)
          (2, 3, 5) ]

    [<Fact>]
    let testPart1 () = part1 cubes |> should equal 64

    [<Fact>]
    let testPart2 () = part2 cubes |> should equal 58

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let cubes = parse input

    part1 cubes |> printfn "part1: %d"
    part2 cubes |> printfn "part2: %d"

    0
