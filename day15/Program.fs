open System.Text.RegularExpressions

open Xunit
open FsUnit.Xunit

type Report =
    { SensorX: int
      SensorY: int
      BeaconX: int
      BeaconY: int }

    member this.distance =
        abs (this.SensorX - this.BeaconX) + abs (this.SensorY - this.BeaconY)

let cover (report: Report) y =
    let d = report.distance
    let dy = abs (y - report.SensorY)

    if d < dy then
        None
    else
        Some(report.SensorX - (d - dy), report.SensorX + (d - dy))

let merge ranges =
    ([], Seq.sort ranges)
    ||> Seq.fold (fun acc (s, t) ->
        assert (s <= t)

        match acc with
        | [] -> [ (s, t) ]
        // s' <= t' < s <= t
        | (s', t') :: l when t' < s ->
            if t' + 1 = s then
                // merge
                (s', t) :: l
            else
                // t' + 1 < s
                (s, t) :: (s', t') :: l
        // s' <= s <= t' <= t
        // s' <= s <= t <= t'
        | (s', t') :: l ->
            // merge
            (s', max t t') :: l)
    |> Seq.rev

let covers reports y =
    reports |> Seq.choose (fun r -> cover r y) |> merge

let part1 reports y =
    covers reports y
    |> Seq.sumBy (fun (s, t) ->
        if Seq.exists (fun r -> s <= r.BeaconX && r.BeaconX <= t) reports then
            // ビーコンの分を除く
            t - s
        else
            t - s + 1)

let part2 reports ymax =
    let beacons =
        [ 0..ymax ]
        |> Seq.choose (fun y ->
            match List.ofSeq (covers reports y) with
            | [ _ ] -> None
            | [ (_, t); (s, _) ] when t + 2 = s -> Some(t + 1, y)
            | _ -> failwith $"y = {y}")

    let x, y = Seq.exactlyOne beacons // センサーに見つからないビーコンはひとつだけ
    int64 x * 4000000L + int64 y

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        let m = Regex.Matches(s, @"(-?\d+)")

        match List.ofSeq m with
        | [ x1; y1; x2; y2 ] ->
            { SensorX = int x1.Value
              SensorY = int y1.Value
              BeaconX = int x2.Value
              BeaconY = int y2.Value }
        | _ -> failwith $"s = {s}")

[<Fact>]
let testCover () =
    let r =
        { SensorX = 8
          SensorY = 7
          BeaconX = 2
          BeaconY = 10 }

    cover r -3 |> should equal None
    cover r -2 |> should equal (Some(8, 8))
    cover r 7 |> should equal (Some(-1, 17))
    cover r 16 |> should equal (Some(8, 8))
    cover r 17 |> should equal None

[<Fact>]
let testMerge () =
    merge [ (0, 5); (3, 7); (4, 4); (9, 10); (11, 12) ]
    |> List.ofSeq
    |> should equal [ (0, 7); (9, 12) ]

module Example =
    let input =
        [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
          "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
          "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
          "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
          "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
          "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
          "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
          "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
          "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
          "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
          "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
          "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
          "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
          "Sensor at x=20, y=1: closest beacon is at x=15, y=3" ]

    let reports = parse input

    [<Fact>]
    let testPart1 () = part1 reports 10 |> should equal 26

    [<Fact>]
    let testReport2 () =
        part2 reports 20 |> should equal 56000011L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let reports = parse input

    part1 reports 2000000 |> printfn "part1: %d"
    part2 reports 4000000 |> printfn "part2: %d" // 2分ほどかかる

    0
