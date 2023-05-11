open System.Text.RegularExpressions

open Xunit
open FsUnit.Xunit

type Procedure = { Move: int; From: int; To: int }

let parseCrates (crates: string) =
    let crates = crates.Split("\n") |> Array.map (Array.ofSeq)
    let n = crates.Length
    let m = crates.[n - 1].Length

    [ 0 .. (m - 1) ]
    |> Seq.choose (fun j ->
        if crates.[n - 1].[j] = ' ' then
            None
        else
            let x = int crates.[n - 1].[j] - int '0'

            let crates =
                [ 0 .. (n - 2) ]
                |> List.rev
                |> List.choose (fun i -> if crates.[i].[j] = ' ' then None else Some crates.[i].[j])
                |> List.rev

            Some(x, crates))
    |> Map.ofSeq

let parseProcedures (procedures: string) =
    procedures.Split("\n")
    |> Seq.map (fun s ->
        let m = Regex.Matches(s, @"\d+")

        match List.ofSeq m with
        | [ a; b; c ] ->
            { Move = int a.Value
              From = int b.Value
              To = int c.Value }
        | _ -> failwith $"s = {s}")

let step (crates: Map<int, char list>) (procedure: Procedure) map =
    let move, stay = crates.[procedure.From] |> List.splitAt procedure.Move
    let to_ = map move @ crates.[procedure.To]
    crates |> Map.add procedure.From stay |> Map.add procedure.To to_

let solve crates procedures map =
    (crates, procedures)
    ||> Seq.fold (fun acc proc -> step acc proc map)
    |> Map.values
    |> Seq.map Seq.head
    |> System.String.Concat

let part1 crates procedures = solve crates procedures List.rev

let part2 crates procedures = solve crates procedures id

let parse (input: string) =
    match input.Split("\n\n") with
    | [| crates; procedures |] -> parseCrates crates, parseProcedures procedures
    | _ -> failwith $"input = {input}"

module Example =
    let input =
        "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
            .TrimStart('\n')

    let crates, procedures = parse input

    [<Fact>]
    let testPart1 () =
        part1 crates procedures |> should equal "CMZ"

    [<Fact>]
    let testPart2 () =
        part2 crates procedures |> should equal "MCD"

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let crates, procedures = parse input

    part1 crates procedures |> printfn "part1: %s"
    part2 crates procedures |> printfn "part2: %s"

    0
