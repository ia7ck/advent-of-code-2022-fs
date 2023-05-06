open Xunit
open FsUnit.Xunit

type Monkey = { Name: string; Expr: Expr }

and Expr =
    | Number of int64
    | BinOp of Op * string * string

and Op =
    | Add
    | Sub
    | Mul
    | Div

let parse (input: string seq) =
    let p (s: string) (sep: char) =
        match s.Split(sep) with
        | [| left; right |] -> Some(left.Trim(), right.Trim())
        | _ -> None

    let pOp s sep op =
        p s sep |> Option.map (fun (l, r) -> BinOp(op, l, r))

    let pAdd s = pOp s '+' Add
    let pSub s = pOp s '-' Sub
    let pMul s = pOp s '*' Mul
    let pDiv s = pOp s '/' Div

    input
    |> Seq.map (fun s ->
        let name, t =
            match s.Split(':') with
            | [| name; t |] -> (name.Trim(), t.Trim())
            | _ -> failwith $"s = {s}"

        let expr =
            pAdd t
            |> Option.orElseWith (fun () -> pSub t)
            |> Option.orElseWith (fun () -> pMul t)
            |> Option.orElseWith (fun () -> pDiv t)
            |> Option.defaultWith (fun () -> Number(int t))

        (name, { Name = name; Expr = expr }))
    |> Map.ofSeq

let eval name monkeys =
    let rec eval_ monkey =
        match monkey.Expr with
        | Number x -> x
        | BinOp(op, l, r) ->
            let lv, rv = eval_ (Map.find l monkeys), eval_ (Map.find r monkeys)

            match op with
            | Add -> lv + rv
            | Sub -> lv - rv
            | Mul -> lv * rv
            | Div -> lv / rv

    let m = Map.find name monkeys
    eval_ m

let part1 monkeys = eval "root" monkeys

let part2 monkeys =
    let rec f monkey =
        if monkey.Name = "humn" then
            Some [ "humn" ]
        else
            match monkey.Expr with
            | Number _ -> None
            | BinOp(_, l, r) ->
                match f (Map.find l monkeys), f (Map.find r monkeys) with
                | Some lv, Some rv -> failwithf "lv = %A, rv = %A" lv rv
                | Some lv, None -> Some(monkey.Name :: lv)
                | None, Some rv -> Some(monkey.Name :: rv)
                | None, None -> None

    let root = Map.find "root" monkeys
    let humanParents = f root |> Option.get |> Set.ofSeq

    let rec g monkey y =
        if monkey.Name = "humn" then
            y
        else
            match monkey.Expr with
            | Number _ -> failwithf "monkey = %A" monkey
            | BinOp(op, l, r) ->
                match Set.contains l humanParents, Set.contains r humanParents with
                | true, false ->
                    let c = eval r monkeys
                    // x op c = y
                    let ny =
                        match op with
                        | Add -> y - c
                        | Sub -> y + c
                        | Mul -> y / c
                        | Div -> y * c

                    g (Map.find l monkeys) ny
                | false, true ->
                    let c = eval l monkeys
                    // c op x = y
                    let ny =
                        match op with
                        | Add -> y - c
                        | Sub -> c - y
                        | Mul -> y / c
                        | Div -> c / y

                    g (Map.find r monkeys) ny
                | _ -> failwith "l = {l}, r = {r}"

    let var, const_ =
        match root.Expr with
        | BinOp(_, l, r) when Set.contains l humanParents -> l, r
        | BinOp(_, l, r) when Set.contains r humanParents -> r, l
        | _ -> failwithf "root = %A" root

    let y = eval const_ monkeys
    g (Map.find var monkeys) y

module Example =
    let input =
        [ "root: pppw + sjmn"
          "dbpl: 5"
          "cczh: sllz + lgvd"
          "zczc: 2"
          "ptdq: humn - dvpt"
          "dvpt: 3"
          "lfqf: 4"
          "humn: 5"
          "ljgn: 2"
          "sjmn: drzm * dbpl"
          "sllz: 4"
          "pppw: cczh / lfqf"
          "lgvd: ljgn * ptdq"
          "drzm: hmdt - zczc"
          "hmdt: 32" ]

    let monkeys = parse input

    [<Fact>]
    let testPart1 () = part1 monkeys |> should equal 152L

    [<Fact>]
    let testPart2 () = part2 monkeys |> should equal 301L

    [<EntryPoint>]
    let main _ =
        let input = stdin.ReadToEnd().TrimEnd().Split("\n")

        let monkeys = parse input

        part1 monkeys |> printfn "part1: %d"
        part2 monkeys |> printfn "part2: %d"
        0
