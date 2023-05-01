open Xunit
open FsUnit.Xunit

type Facing =
    | Right // >
    | Down // v
    | Left // <
    | Up // ^

let toInt f =
    match f with
    | Right -> 0
    | Down -> 1
    | Left -> 2
    | Up -> 3

let turnLeft f =
    match f with
    | Right -> Up
    | Down -> Right
    | Left -> Down
    | Up -> Left

let turnRight f =
    match f with
    | Right -> Down
    | Down -> Left
    | Left -> Up
    | Up -> Right

type Instrunction =
    | TurnLeft
    | TurnRight
    | Move of int

let password row column facing =
    1000 * (row + 1) + 4 * (column + 1) + toInt facing

let movePart1 (map: char array array) (i, j, f: Facing) =
    let right =
        let nj =
            Seq.initInfinite (fun d -> (j + d + 1) % map.[i].Length)
            |> Seq.find (fun nj -> map.[i].[nj] <> ' ')

        if map.[i].[nj] = '.' then
            (i, nj)
        else
            assert (map.[i].[nj] = '#')
            (i, j)

    let down =
        let ni =
            Seq.initInfinite (fun d -> (i + d + 1) % map.Length)
            |> Seq.find (fun ni -> map.[ni].[j] <> ' ')

        if map.[ni].[j] = '.' then
            (ni, j)
        else
            assert (map.[ni].[j] = '#')
            (i, j)

    let left =
        let w = map.[i].Length

        let nj =
            Seq.initInfinite (fun d -> (w + j - (d + 1)) % w)
            |> Seq.find (fun nj -> map.[i].[nj] <> ' ')

        if map.[i].[nj] = '.' then
            (i, nj)
        else
            assert (map.[i].[nj] = '#')
            (i, j)


    let up =
        let h = map.Length

        let ni =
            Seq.initInfinite (fun d -> (h + i - (d + 1)) % h)
            |> Seq.find (fun ni -> map.[ni].[j] <> ' ')

        if map.[ni].[j] = '.' then
            (ni, j)
        else
            assert (map.[ni].[j] = '#')
            (i, j)

    let (ni, nj) =
        match f with
        | Right -> right
        | Down -> down
        | Left -> left
        | Up -> up

    (ni, nj, f)

let movePart2 (map: char array array) (i, j, f: Facing) =
    // どんな展開図が来ても動くようにするのは大変そうなので
    // とりあえずこの形の展開図だけ扱う

    //    111222
    //    111222
    //    111222
    //    333
    //    333
    //    333
    // 444555
    // 444555
    // 444555
    // 666
    // 666
    // 666

    let n = 50

    let s =
        if i < n && n <= j && j < n * 2 then
            1
        else if i < n && n * 2 <= j then
            2
        else if n <= i && i < n * 2 && n <= j && j < n * 2 then
            3
        else if n * 2 <= i && i < n * 3 && j < n then
            4
        else if n * 2 <= i && i < n * 3 && n <= j && j < n * 2 then
            5
        else if n * 3 <= i && j < n then
            6
        else
            failwith $"i = {i}, j = ${j}"

    let (ni, nj, nf) =
        match s, f with
        | 1, Right -> (i, j + 1, f)
        | 1, Down -> (i + 1, j, f)
        | 1, Left ->
            if j - 1 >= n then
                (i, j - 1, f)
            else
                (n * 3 - 1 - i, 0, Right)
        | 1, Up -> if i - 1 >= 0 then (i - 1, j, f) else (j + n * 2, 0, Right)
        | 2, Right ->
            if j + 1 < n * 3 then
                (i, j + 1, f)
            else
                (n * 3 - 1 - i, n * 2 - 1, Left)
        | 2, Down ->
            if i + 1 < n then
                (i + 1, j, f)
            else
                (j - n, n * 2 - 1, Left)
        | 2, Left -> (i, j - 1, f)
        | 2, Up ->
            if i - 1 >= 0 then
                (i - 1, j, f)
            else
                (n * 4 - 1, j - n * 2, Up)
        | 3, Right -> if j + 1 < n * 2 then (i, j + 1, f) else (n - 1, i + n, Up)
        | 3, Down -> (i + 1, j, f)
        | 3, Left -> if j - 1 >= n then (i, j - 1, f) else (n * 2, i - n, Down)
        | 3, Up -> (i - 1, j, f)
        | 4, Right -> (i, j + 1, f)
        | 4, Down -> (i + 1, j, f)
        | 4, Left ->
            if j - 1 >= 0 then
                (i, j - 1, f)
            else
                (n * 3 - 1 - i, n, Right)
        | 4, Up -> if i - 1 >= n * 2 then (i - 1, j, f) else (j + n, n, Right)
        | 5, Right ->
            if j + 1 < n * 2 then
                (i, j + 1, f)
            else
                (n * 3 - 1 - i, n * 3 - 1, Left)
        | 5, Down ->
            if i + 1 < n * 3 then
                (i + 1, j, f)
            else
                (j + n * 2, n - 1, Left)
        | 5, Left -> (i, j - 1, f)
        | 5, Up -> (i - 1, j, f)
        | 6, Right ->
            if j + 1 < n then
                (i, j + 1, f)
            else
                (n * 3 - 1, i - n * 2, Up)
        | 6, Down ->
            if i + 1 < n * 4 then
                (i + 1, j, f)
            else
                (0, j + n * 2, Down)
        | 6, Left -> if j - 1 >= 0 then (i, j - 1, f) else (0, i - n * 2, Down)
        | 6, Up -> (i - 1, j, f)
        | _ -> failwith $"s = ${s}, f = ${f}"

    assert (map.[ni].[nj] <> ' ')
    if map.[ni].[nj] = '.' then (ni, nj, nf) else (i, j, f)

let next (map: char array array) (i, j, f: Facing) instruction move =
    match instruction with
    | TurnLeft -> (i, j, turnLeft f)
    | TurnRight -> (i, j, turnRight f)
    | Move(d) ->
        ((i, j, f), Seq.init d (fun _ -> ()))
        ||> Seq.fold (fun (i', j', f') _ -> move map (i', j', f'))

// [number][LR][number][LR]...[LR][number]
let rec parse (instructions: string) =
    match instructions.IndexOf('L'), instructions.IndexOf('R') with
    | -1, -1 -> [ Move(int instructions) ]
    | i, j when j = -1 || i <> -1 && i < j ->
        assert (i >= 1)

        Move(int (instructions.Substring(0, i)))
        :: TurnLeft
        :: parse (instructions.Substring(i + 1))
    | i, j when i = -1 || j <> -1 && i > j ->
        assert (j >= 1)

        Move(int (instructions.Substring(0, j)))
        :: TurnRight
        :: parse (instructions.Substring(j + 1))
    | _ -> failwith $"instructions = {instructions}"

let solve (map: char array array) instructions move =
    let sj = map.[0] |> Seq.findIndex ((=) '.')

    let (i, j, f) =
        ((0, sj, Right), instructions)
        ||> Seq.fold (fun (i, j, f) instruction -> next map (i, j, f) instruction move)

    password i j f

[<Fact>]
let movePart1Test () =
    let map = [| "  ..#..  " |] |> Array.map Seq.toArray
    movePart1 map (0, 2, Right) |> should equal (0, 3, Right)
    movePart1 map (0, 3, Right) |> should equal (0, 3, Right)
    movePart1 map (0, 5, Right) |> should equal (0, 6, Right)
    movePart1 map (0, 6, Right) |> should equal (0, 2, Right)

[<Fact>]
let parseTest () =
    parse "123L456R789"
    |> should equal [ Move 123; TurnLeft; Move 456; TurnRight; Move 789 ]

[<Fact>]
let test () =
    let map =
        [| "        ...#    "
           "        .#..    "
           "        #...    "
           "        ....    "
           "...#.......#    "
           "........#..."
           "..#....#....    "
           "..........#.    "
           "        ...#...."
           "        .....#.."
           "        .#......"
           "        ......#." |]
        |> Array.map Seq.toArray

    solve map (parse "10R5L5R10L4R5L5") movePart1 |> should equal 6032

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let map = input[.. input.Length - 3]
    let width = map |> Array.map Seq.length |> Array.max

    let map =
        map
        // 右側はスペースで埋めておく
        |> Array.map (fun s -> s + (String.replicate (width - s.Length) " "))
        |> Array.map Seq.toArray

    assert (Array.forall (fun a -> Array.length a = width) map)

    let instructions = input[input.Length - 1]

    solve map (parse instructions) movePart1 |> printfn "part1: %d"
    solve map (parse instructions) movePart2 |> printfn "part2: %d"
    0
