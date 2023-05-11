open Xunit
open FsUnit.Xunit

let priority c =
    if 'a' <= c && c <= 'z' then int c - int 'a' + 1
    else if 'A' <= c && c <= 'Z' then int c - int 'A' + 27
    else failwith $"c = {c}"

let part1 (rucksacks: string seq) =
    rucksacks
    |> Seq.sumBy (fun rucksack ->
        let front, back = List.splitAt (rucksack.Length / 2) (List.ofSeq rucksack)
        Set.intersect (Set.ofList front) (Set.ofList back) |> Seq.exactlyOne |> priority)

let part2 (rucksacks: string seq) =
    rucksacks
    |> Seq.chunkBySize 3
    |> Seq.sumBy (fun rucksacks -> Set.intersectMany (Seq.map Set.ofSeq rucksacks) |> Seq.exactlyOne |> priority)

module Example =
    let input =
        [ "vJrwpWtwJgWrhcsFMMfFFhFp"
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
          "PmmdzqPrVvPwwTWBwg"
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
          "ttgJtRGJQctTZtZT"
          "CrZsJsPPZsGzwwsLwLmpwMDw" ]

    [<Fact>]
    let testPart1 () = part1 input |> should equal 157

    [<Fact>]
    let testPart2 () = part2 input |> should equal 70


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")

    part1 input |> printfn "part1: %d"
    part2 input |> printfn "part2: %d"

    0
