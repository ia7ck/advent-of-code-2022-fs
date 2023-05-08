open Xunit
open FsUnit.Xunit

[<RequireQualifiedAccess>]
type Value =
    | Int of int
    | List of Value list

let pInt s =
    let isDigit c = '0' <= c && c <= '9'
    let n = List.takeWhile isDigit s |> Array.ofList |> System.String.Concat |> int
    Value.Int n, List.skipWhile isDigit s

let rec pValue s =
    match s with
    | c :: s' when '0' <= c && c <= '9' -> pInt (c :: s')
    | '[' :: ']' :: s' -> Value.List [], s'
    | '[' :: s' ->
        match pCommaSeparated s' with
        | v, ']' :: t -> Value.List v, t
        // 閉じかっこがない
        | _ -> failwithf "s = %A" s
    // 数値でもリストでもない
    | _ -> failwithf "s = %A" s


and pCommaSeparated s =
    let rec p acc s =
        match pValue s with
        // カンマを読み飛ばして再帰
        | v, ',' :: s' -> p (v :: acc) s'
        // カンマがなかったら終わり
        | v, s' -> List.rev (v :: acc), s'

    p [] s

let parse s =
    let v, rest = pValue (List.ofSeq s)
    assert (List.isEmpty rest) // パースしそこねたテキストがない
    v

let rec compareValue l r =
    match l, r with
    | Value.Int l, Value.Int r -> compare l r
    | Value.List l, Value.List r ->
        match l, r with
        | [], [] -> 0 // equal
        | [], _h :: _t -> -1
        | _h :: _t, [] -> 1
        | lh :: l, rh :: r ->
            let c = compareValue lh rh

            if c <> 0 then
                c
            else
                compareValue (Value.List l) (Value.List r)
    | Value.Int l, Value.List r -> compareValue (Value.List [ Value.Int l ]) (Value.List r)
    | Value.List l, Value.Int r -> compareValue (Value.List l) (Value.List [ Value.Int r ])

let part1 (input: string) =
    let packetPairs =
        input.Split("\n\n")
        |> Seq.map (fun s ->
            match s.Split("\n") with
            | [| l; r |] -> parse l, parse r
            | _ -> failwith $"s = {s}")

    packetPairs
    |> Seq.indexed
    |> Seq.filter (fun (_, (l, r)) -> compareValue l r < 0)
    |> Seq.map (fun (i, _) -> i + 1)
    |> Seq.sum

let part2 (input: string) =
    let p2 = parse "[[2]]"
    let p6 = parse "[[6]]"

    let packets =
        input.Split("\n\n")
        |> Seq.collect (fun s ->
            match s.Split("\n") with
            | [| l; r |] -> [ parse l; parse r ]
            | _ -> failwith $"s = {s}")
        |> Seq.append [ p2; p6 ]
        |> Seq.sortWith compareValue

    let p2Index = Seq.findIndex ((=) p2) packets
    let p6Index = Seq.findIndex ((=) p6) packets

    (p2Index + 1) * (p6Index + 1)

[<Fact>]
let testParse () =
    parse "12" |> should equal (Value.Int 12)
    parse "[]" |> should equal (Value.List [])
    parse "[12]" |> should equal (Value.List [ Value.Int 12 ])

    parse "[12,34]" |> should equal (Value.List [ Value.Int 12; Value.Int 34 ])

    parse "[[12,34],56,[78]]"
    |> should
        equal
        (Value.List
            [ Value.List [ Value.Int 12; Value.Int 34 ]
              Value.Int 56
              Value.List [ Value.Int 78 ] ])

module Example =
    let input =
        """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

    [<Fact>]
    let testPart1 () = part1 input |> should equal 13

    [<Fact>]
    let testPart2 () = part2 input |> should equal 140

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()

    part1 input |> printfn "part1: %d"
    part2 input |> printfn "part2: %d"

    0
