open Xunit
open FsUnit.Xunit

let parseIntInt (input: string) (sep: string) =
    match input.Split(sep) with
    | [| l; r |] -> int l, int r
    | _ -> failwith $"input = {input}"

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        match s.Split(",") with
        | [| l; r |] -> parseIntInt l "-", parseIntInt r "-"
        | _ -> failwith $"s = {s}")

let part1 pairs =
    pairs
    |> Seq.filter (fun ((l1, r1), (l2, r2)) ->
        let s1 = Set.ofList [ l1..r1 ]
        let s2 = Set.ofList [ l2..r2 ]
        Set.isSubset s1 s2 || Set.isSubset s2 s1)
    |> Seq.length

let part2 pairs =
    pairs
    |> Seq.filter (fun ((l1, r1), (l2, r2)) ->
        let s1 = Set.ofList [ l1..r1 ]
        let s2 = Set.ofList [ l2..r2 ]
        Set.intersect s1 s2 |> Set.isEmpty |> not)
    |> Seq.length

module Example =
    let input = [ "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8" ]
    let pairs = parse input

    [<Fact>]
    let testPart1 () = part1 pairs |> should equal 2

    [<Fact>]
    let testPart2 () = part2 pairs |> should equal 4

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let pairs = parse input

    part1 pairs |> printfn "part1: %d"
    part2 pairs |> printfn "part2: %d"

    0
