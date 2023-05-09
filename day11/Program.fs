open Xunit
open FsUnit.Xunit

type Decision =
    { Operation: Op
      TestDivisor: int64
      IfTrue: int
      IfFalse: int }

    member this.DoOperation(worryLevel) =
        match this.Operation with
        | Add x -> worryLevel + x
        | Mul x -> worryLevel * x
        | Sq -> worryLevel * worryLevel

    member this.Throw(worryLevel) =
        if worryLevel % this.TestDivisor = 0L then
            this.IfTrue
        else
            this.IfFalse

and Op =
    | Add of int64
    | Mul of int64
    | Sq

let parseMonkey (s: string) =
    s.Replace("Monkey ", "").TrimEnd(':') |> int

let parseWorryLevels (s: string) =
    s.Replace("  Starting items: ", "").Split(", ") |> Seq.map int64

let parseOperation (s: string) =
    match s.Replace("  Operation: new = ", "") with
    | "old * old" -> Sq
    | t when t.StartsWith("old * ") -> t.Replace("old * ", "") |> int64 |> Mul
    | t when t.StartsWith("old + ") -> t.Replace("old + ", "") |> int64 |> Add
    | _ -> failwith $"s = {s}"

let parseTestDivisor (s: string) =
    s.Replace("  Test: divisible by ", "") |> int64

let parseIfTrue (s: string) =
    s.Replace("    If true: throw to monkey ", "") |> int

let parseIfFalse (s: string) =
    s.Replace("    If false: throw to monkey ", "") |> int

let parse (input: string) =
    let monkeys, worryLevels, decisions =
        input.Split("\n\n")
        |> List.ofArray
        |> List.map (fun s ->
            match s.Split("\n") with
            | [| monkey; worryLevels; operation; testDivisor; ifTrue; ifFalse |] ->
                let monkey = parseMonkey monkey
                let testDivisor = parseTestDivisor testDivisor

                (monkey,
                 parseWorryLevels worryLevels,
                 { Operation = parseOperation operation
                   TestDivisor = testDivisor
                   IfTrue = parseIfTrue ifTrue
                   IfFalse = parseIfFalse ifFalse })
            | _ -> failwith $"s = {s}")
        |> List.unzip3

    (List.zip monkeys worryLevels), Map.ofList (List.zip monkeys decisions)

let stepRound (decisions: Map<int, Decision>) (modifier: int64 -> int64) (worryLevels: (int * int64) seq) =
    let rec f worryLevels inspectCount out =
        if Seq.isEmpty worryLevels then
            inspectCount, out
        else
            let nextInspectCount =
                (inspectCount, worryLevels)
                ||> Seq.fold (fun acc (monkey, _) ->
                    acc |> Map.change monkey (fun c -> Some(1 + Option.defaultValue 0 c)))

            let nextWorryLevels, nextOut =
                (([], out), worryLevels)
                ||> Seq.fold (fun (nextWorryLevels, out') (monkey, level) ->
                    let decision = Map.find monkey decisions
                    let nextLevel = decision.DoOperation(level) |> modifier
                    let nextMonkey = decision.Throw(nextLevel)
                    let n = nextMonkey, nextLevel

                    if nextMonkey > monkey then
                        n :: nextWorryLevels, out'
                    else
                        nextWorryLevels, n :: out')

            f nextWorryLevels nextInspectCount nextOut

    f (List.ofSeq worryLevels) Map.empty []

let solve worryLevels decisions modifier round =
    let worryLevels =
        worryLevels
        |> Seq.collect (fun (monkey, levels) -> levels |> Seq.map (fun l -> (monkey, l)))

    ((Map.empty, worryLevels), [ 1..round ])
    ||> Seq.fold (fun (count', worryLevels') _r ->
        let count, worryLevels = stepRound decisions modifier worryLevels'

        let merged =
            (count', count)
            ||> Map.fold (fun acc k v -> acc |> Map.change k (fun v' -> Some(v + Option.defaultValue 0 v')))

        merged, worryLevels)

let monkeyBusinessLevel inspectCount =
    inspectCount
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.map int64
    |> Seq.reduce (*)

let part1 worryLevels decisions =
    let div3 = fun x -> x / 3L
    let count, _ = solve worryLevels decisions div3 20
    monkeyBusinessLevel count

let part2 worryLevels decisions =
    let rec gcd a b = if b = 0L then a else gcd b (a % b)
    let lcm a b = a / (gcd a b) * b

    let l = Map.values decisions |> Seq.map (fun d -> d.TestDivisor) |> Seq.reduce lcm
    let modLcm = fun x -> x % l

    let count, _ = solve worryLevels decisions modLcm 10000
    monkeyBusinessLevel count

module Example =
    let input =
        "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"

    let worryLevels, decisions = parse input

    [<Fact>]
    let testPart1 () =
        part1 worryLevels decisions |> should equal 10605L

    [<Fact>]
    let testPart2 () =
        part2 worryLevels decisions |> should equal 2713310158L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let worryLevels, decisions = parse input

    part1 worryLevels decisions |> printfn "part1: %d"
    part2 worryLevels decisions |> printfn "part2: %d"

    0
