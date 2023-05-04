open Xunit
open FsUnit.Xunit

let next round (elves: (int * int) Set) =
    let empty points =
        points |> Seq.forall (fun (ni, nj) -> not (Set.contains (ni, nj) elves))

    let purpose (i, j) =
        [ {| Purpose = (i - 1, j)
             Ok = empty [ (i - 1, j - 1); (i - 1, j); (i - 1, j + 1) ] |}
          {| Purpose = (i + 1, j)
             Ok = empty [ (i + 1, j - 1); (i + 1, j); (i + 1, j + 1) ] |}
          {| Purpose = (i, j - 1)
             Ok = empty [ (i - 1, j - 1); (i, j - 1); (i + 1, j - 1) ] |}
          {| Purpose = (i, j + 1)
             Ok = empty [ (i - 1, j + 1); (i, j + 1); (i + 1, j + 1) ] |} ]
        |> List.permute (fun i -> (i + (4 - round % 4)) % 4)
        |> List.tryPick (fun x -> if x.Ok then Some x.Purpose else None)

    elves
    |> Seq.map (fun (i, j) ->
        let fix =
            empty
                [ (i - 1, j)
                  (i - 1, j - 1)
                  (i, j - 1)
                  (i + 1, j - 1)
                  (i + 1, j)
                  (i + 1, j + 1)
                  (i, j + 1)
                  (i - 1, j + 1) ]

        let purpose =
            if fix then
                (i, j)
            else
                purpose (i, j) |> Option.defaultValue (i, j)

        {| Current = (i, j)
           Purpose = purpose |})
    |> Seq.groupBy (fun x -> x.Purpose)
    |> Seq.collect (fun (purpose, xs) ->
        if Seq.length xs = 1 then
            seq { purpose }
        else
            xs |> Seq.map (fun x -> x.Current))
    |> Set.ofSeq

let boundingRectangleSize (elves: (int * int) seq) =
    let top = elves |> Seq.map fst |> Seq.min
    let bottom = elves |> Seq.map fst |> Seq.max
    let left = elves |> Seq.map snd |> Seq.min
    let right = elves |> Seq.map snd |> Seq.max

    (bottom - top + 1) * (right - left + 1)

let debug (elves: (int * int) seq) =
    let top = elves |> Seq.map fst |> Seq.min
    let bottom = elves |> Seq.map fst |> Seq.max
    let left = elves |> Seq.map snd |> Seq.min
    let right = elves |> Seq.map snd |> Seq.max

    for i in top..bottom do
        for j in left..right do
            let ch = if Seq.contains (i, j) elves then '#' else '.'
            printf "%c" ch

        printf "\n"

let parse (input: char array array) =
    let h, w = input.Length, input.[0].Length

    let elves =
        seq {
            for i in 0 .. (h - 1) do
                for j in 0 .. (w - 1) do
                    if input.[i].[j] = '#' then
                        yield (i, j)
        }

    elves

let part1 (elves: (int * int) seq) =
    (Set.ofSeq elves, [ 0..9 ])
    ||> Seq.fold (fun acc round -> next round acc)
    |> boundingRectangleSize
    |> fun size -> size - Seq.length elves

let part2 (elves: (int * int) seq) =
    let rec loop round elves =
        let n = next round elves
        if n = elves then round else loop (round + 1) n

    (loop 0 (Set.ofSeq elves)) + 1

module Example =
    let input =
        [| ".............."
           ".............."
           ".......#......"
           ".....###.#...."
           "...#...#.#...."
           "....#...##...."
           "...#.###......"
           "...##.#.##...."
           "....#..#......"
           ".............."
           ".............."
           ".............." |]
        |> Array.map Seq.toArray

    [<Fact>]
    let testPart1 () =
        let elves = parse input
        part1 elves |> should equal 110

    [<Fact>]
    let testPart2 () =
        let elves = parse input
        part2 elves |> should equal 20


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")

    let elves = input |> Array.map Seq.toArray |> parse
    part1 elves |> printfn "part1: %d"
    part2 elves |> printfn "part2: %d"

    0
