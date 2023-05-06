open Xunit
open FsUnit.Xunit

let inline (%%) x y = (x % y + y) % y

type V = { Value: int64; Order: int }

let next (a: V list) ord =
    let n = List.length a
    let i = List.findIndex (fun v -> v.Order = ord) a
    let v = List.item i a

    a
    |> List.removeAt i
    |> List.insertAt (int ((int64 i + v.Value) %% (int64 (n - 1)))) v

let hash (a: V list) =
    let i = List.findIndex (fun v -> v.Value = 0L) a

    [ 1000; 2000; 3000 ]
    |> List.sumBy (fun j -> let v = List.item ((i + j) % a.Length) a in v.Value)

let part1 a =
    let a = a |> List.indexed |> List.map (fun (i, x) -> { Value = x; Order = i })
    let b = (a, [ 0 .. (List.length a - 1) ]) ||> List.fold next

    hash b

let part2 a =
    let a =
        a
        |> List.indexed
        |> List.map (fun (i, x) -> { Value = x * 811589153L; Order = i })

    let b =
        (a, List.replicate 10 [ 0 .. (List.length a - 1) ] |> List.concat)
        ||> List.fold next

    hash b

let parse input = input |> Seq.map int64 |> List.ofSeq

module Example =
    let a = [ 1; 2; -3; 3; -2; 0; 4 ] |> List.map int64

    [<Fact>]
    let testPart1 () = part1 a |> should equal 3L

    [<Fact>]
    let testPart2 () = part2 a |> should equal 1623178306L


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let a = parse input

    part1 a |> printfn "part1: %d"
    part2 a |> printfn "part2: %d"

    0
