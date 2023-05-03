open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit

type OreRobotCost = { Ore: int }
type ClayRobotCost = { Ore: int }
type ObsidianRobotCost = { Ore: int; Clay: int }
type GeodeRobotCost = { Ore: int; Obsidian: int }

type Blueprint =
    { OreRobotCost: OreRobotCost
      ClayRobotCost: ClayRobotCost
      ObsidianRobotCost: ObsidianRobotCost
      GeodeRobotCost: GeodeRobotCost }

let solve (bp: Blueprint) time =
    let mutable best = 0

    let rec f t (oreRobot, clayRobot, obsidianRobot, geodeRobot) (ore, clay, obsidian, geode) =
        if t = 0 then
            if best < geode then
                best <- geode
        else if geode + geodeRobot * t + t * (t - 1) / 2 <= best then
            // 枝刈り
            // 残り t 分間を最高効率で geode に充てたとしても best を超えられないなら探索を打ち切り
            ()
        else
            let (nextOre, nextClay, nextObsidian, nextGeode) =
                (ore + oreRobot, clay + clayRobot, obsidian + obsidianRobot, geode + geodeRobot)

            if bp.GeodeRobotCost.Ore <= ore && bp.GeodeRobotCost.Obsidian <= obsidian then
                f
                    (t - 1)
                    (oreRobot, clayRobot, obsidianRobot, geodeRobot + 1)
                    (nextOre - bp.GeodeRobotCost.Ore, nextClay, nextObsidian - bp.GeodeRobotCost.Obsidian, nextGeode)

            if bp.ObsidianRobotCost.Ore <= ore && bp.ObsidianRobotCost.Clay <= clay then
                f
                    (t - 1)
                    (oreRobot, clayRobot, obsidianRobot + 1, geodeRobot)
                    (nextOre - bp.ObsidianRobotCost.Ore, nextClay - bp.ObsidianRobotCost.Clay, nextObsidian, nextGeode)

            if bp.ClayRobotCost.Ore <= ore then
                f
                    (t - 1)
                    (oreRobot, clayRobot + 1, obsidianRobot, geodeRobot)
                    (nextOre - bp.ClayRobotCost.Ore, nextClay, nextObsidian, nextGeode)

            if bp.OreRobotCost.Ore <= ore then
                f
                    (t - 1)
                    (oreRobot + 1, clayRobot, obsidianRobot, geodeRobot)
                    (nextOre - bp.OreRobotCost.Ore, nextClay, nextObsidian, nextGeode)

            f (t - 1) (oreRobot, clayRobot, obsidianRobot, geodeRobot) (nextOre, nextClay, nextObsidian, nextGeode)

    f time (1, 0, 0, 0) (0, 0, 0, 0)
    best

let parse input =
    let m = Regex.Matches(input, @"\d+")

    match List.ofSeq m with
    | [ a; b; c; d; e; f; g ] ->
        (int a.Value,
         { OreRobotCost = { Ore = int b.Value }
           ClayRobotCost = { Ore = int c.Value }
           ObsidianRobotCost =
             { Ore = int d.Value
               Clay = int e.Value }
           GeodeRobotCost =
             { Ore = int f.Value
               Obsidian = int g.Value } })
    | _ -> failwithf "input = %s, matches = %A" input m

let part1 (blueprints: (int * Blueprint) seq) =
    blueprints
    |> Seq.sumBy (fun (id_, blueprint) ->
        let geode = solve blueprint 24
        eprintfn "id = %d, geode = %d" id_ geode
        geode * id_)

let part2 (blueprints: Blueprint seq) =
    blueprints
    |> Seq.fold
        (fun prod blueprint ->
            let geode = solve blueprint 32
            printfn "geode = %d" geode
            prod * geode)
        1

module Example =
    let input =
        [ "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
          "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian." ]

    [<Fact>]
    let testPart1 () =
        let blueprints = Seq.map parse input
        part1 blueprints |> should equal 33

    [<Fact>]
    let testPart2 () =
        let blueprints = Seq.map (parse >> snd) input
        part2 blueprints |> should equal (56 * 62)


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let blueprints = Seq.map parse input

    part1 blueprints |> printfn "part1: %d"

    let blueprints = blueprints |> Seq.take 3 |> Seq.map snd
    part2 blueprints |> printfn "part2: %d"
    0
