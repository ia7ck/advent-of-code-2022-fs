open System.Text.RegularExpressions
open System.Collections.Generic

open Xunit
open FsUnit.Xunit

type Node =
    { Name: string
      Rate: int
      Towards: string seq }

type Edge = { To: string; Weight: int }
type Graph = Map<string, Edge seq>

// 残り時間 t で
// いま頂点 u にいて
// capable の頂点にあるバルブを開けられるときの圧力最大値
// 状態数 26 * 16 * 2^16 = 27262976
let rec solve
    (graph: Graph)
    (rate: Map<string, int>)
    (t: int)
    (u: string)
    (capable: Set<string>)
    (memo: Dictionary<_, int>)
    =
    if t = 0 then
        0
    else if memo.ContainsKey(t, u, capable) then
        memo.Item(t, u, capable)
    else
        let edges = Map.find u graph
        let r = Map.find u rate

        let answer =
            edges
            |> Seq.collect (fun e ->
                seq {
                    // u のバルブは開けずに e.To に移る
                    if u <> e.To && t >= e.Weight then
                        yield solve graph rate (t - e.Weight) e.To capable memo

                    // u のバルブを開けて e.To に移る
                    if t - 1 >= e.Weight && Set.contains u capable then
                        // u のバルブは t-1 分間動く
                        yield
                            r * (t - 1)
                            + (solve graph rate (t - 1 - e.Weight) e.To (Set.remove u capable) memo)
                })
            |> Seq.append [ 0 ]
            |> Seq.max

        memo.Add((t, u, capable), answer)
        answer

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        // tunnel が複数形になったりする
        let m =
            Regex.Match(s, @"^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)$")

        match List.ofSeq m.Groups with
        | [ _; u; rate; vl ] ->
            let u = u.Value.Trim() in

            { Name = u
              Rate = int (rate.Value.Trim())
              Towards = vl.Value.Split(", ") }
        | _ -> failwith $"s = {s}")


let compress nodes =
    let adj = nodes |> Seq.map (fun n -> (n.Name, n.Towards)) |> Map.ofSeq

    // 距離を table に入れていく
    let rec f l d table =
        let l' =
            l
            |> Seq.collect (fun v -> Map.find v adj)
            |> Seq.filter (fun v -> not (Map.containsKey v table))

        if Seq.isEmpty l' then
            table
        else
            let table' = (table, l') ||> Seq.fold (fun acc v -> Map.add v (d + 1) acc)
            f l' (d + 1) table'

    // 実は n.Rate = 0 が多く Seq.length ns <= 16
    let ns = nodes |> Seq.filter (fun n -> n.Name = "AA" || n.Rate >= 1)

    let graph =
        ns
        |> Seq.map (fun n ->
            let distance = f [ n.Name ] 0 (Map [ (n.Name, 0) ])

            let edges =
                distance
                |> Map.filter (fun k _ -> Seq.exists (fun n -> n.Name = k) ns)
                |> Seq.map (fun pair -> { To = pair.Key; Weight = pair.Value })

            (n.Name, edges))
        |> Map.ofSeq

    let rate = ns |> Seq.map (fun n -> n.Name, n.Rate) |> Map.ofSeq

    graph, rate

// 部分列とその complement を列挙
let rec sublists xs =
    match xs with
    | [] -> [ [], [] ]
    | x :: t ->
        let ys, ys' = sublists t |> List.unzip
        let zs = ys |> List.map (fun l -> x :: l)
        let zs' = ys' |> List.map (fun l -> x :: l)
        List.append (List.zip ys zs') (List.zip zs ys')

let part1 nodes =
    let graph, rate = compress nodes
    let memo = Dictionary()
    solve graph rate 30 "AA" (Set.ofSeq graph.Keys) memo

let part2 nodes =
    let graph, rate = compress nodes

    let memo = Dictionary()

    Map.keys graph
    |> List.ofSeq
    |> sublists
    |> Seq.map (fun (human, elephant) ->
        // 人間の担当範囲と象の担当範囲を被らないように
        (solve graph rate 26 "AA" (Set.ofList human) memo)
        + (solve graph rate 26 "AA" (Set.ofList elephant) memo))
    |> Seq.max

[<Fact>]
let testSublists () =
    sublists [ 1 ] |> List.sort |> should equal [ ([], [ 1 ]); ([ 1 ], []) ]

    sublists [ 1; 2; 3 ]
    |> List.sort
    |> should
        equal
        [ ([], [ 1; 2; 3 ])
          ([ 1 ], [ 2; 3 ])
          ([ 1; 2 ], [ 3 ])
          ([ 1; 2; 3 ], [])
          ([ 1; 3 ], [ 2 ])
          ([ 2 ], [ 1; 3 ])
          ([ 2; 3 ], [ 1 ])
          ([ 3 ], [ 1; 2 ]) ]

module Example =
    let input =
        [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
          "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
          "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
          "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
          "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
          "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
          "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
          "Valve HH has flow rate=22; tunnel leads to valve GG"
          "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
          "Valve JJ has flow rate=21; tunnel leads to valve II" ]

    let nodes = parse input

    [<Fact>]
    let testPart1 () = part1 nodes |> should equal 1651

    [<Fact>]
    let testPart2 () = part2 nodes |> should equal 1707

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let nodes = parse input

    part1 nodes |> printfn "part1: %d"
    part2 nodes |> printfn "part2: %d" // 20分ほどかかる

    0
