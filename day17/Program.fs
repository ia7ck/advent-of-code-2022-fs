open Xunit
open FsUnit.Xunit

let W = 7

type Jet =
    | L
    | R

let rocks =
    [|
       // ####
       [ (0, 0); (0, 1); (0, 2); (0, 3) ]

       // .#.
       // ###
       // .#.
       [ (0, 1); (1, 0); (1, 1); (1, 2); (2, 1) ]

       // ..#
       // ..#
       // ###
       [ (0, 0); (0, 1); (0, 2); (1, 2); (2, 2) ]

       // #
       // #
       // #
       // #
       [ (0, 0); (1, 0); (2, 0); (3, 0) ]

       // ##
       // ##
       [ (0, 0); (0, 1); (1, 0); (1, 1) ] |]

let push jet rock chamber =
    let nextRock =
        rock
        |> Seq.map (fun (i, j) ->
            (i,
             match jet with
             | L -> j - 1
             | R -> j + 1))

    if Seq.forall (fun (i, j) -> 0 <= j && j < W && not (Set.contains (i, j) chamber)) nextRock then
        nextRock
    else
        rock

let fall rock chamber =
    let nextRock = rock |> Seq.map (fun (i, j) -> (i - 1, j))

    if Seq.forall (fun (i, j) -> i >= 0 && not (Set.contains (i, j) chamber)) nextRock then
        Some nextRock
    else
        None

let top chamber =
    if Seq.isEmpty chamber then
        0
    else
        1 + (chamber |> Seq.map fst |> Seq.max)

let debug chamber =
    let top = top chamber

    [ 0 .. (top - 1) ]
    |> List.map (fun i ->
        [ 0 .. (W - 1) ]
        |> List.map (fun j -> if Seq.contains (i, j) chamber then "#" else "."))
    |> List.rev
    |> List.iter (fun row -> row |> String.concat "" |> printfn "%s")

let rec cycle xs =
    seq {
        yield! xs
        yield! cycle xs
    }

let solve (rocks: (int * int) list array) (jets: Jet array) n =
    let rec f n' j chamber heights =
        if n' = n then
            List.rev heights
        else
            let rec g rock j' =
                let jet = jets.[j' % jets.Length]
                let rock = push jet rock chamber

                match fall rock chamber with
                | Some nextRock -> g nextRock (j' + 1)
                | None -> rock, (j' + 1)

            let di = top chamber
            let rock = rocks.[n' % rocks.Length] |> Seq.map (fun (i, j) -> (i + di + 3, j + 2))

            let rock', j' = g rock j
            let chamber' = Set.union chamber (Set.ofSeq rock')
            let h = top chamber'
            f (n' + 1) j' chamber' (h :: heights)

    f 0 0 Set.empty []

let parse input =
    input
    |> Seq.map (function
        | '<' -> L
        | '>' -> R
        | ch -> failwithf "%A" ch)
    |> Array.ofSeq

let part1 jets = solve rocks jets 2022 |> List.last

module Example =
    let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    let jets = parse input

    [<Fact>]
    let testPart1 () = part1 jets |> should equal 3068

    [<Fact>]
    let testPart2 () =
        // let heights = solve rocks jets 3000 // 適当な大きめの n を使う
        // (0 :: heights, heights)
        // ||> Seq.zip
        // |> Seq.map (fun (prev, curr) -> curr - prev)
        // |> Seq.iter (printf "%d,")

        // 階差を取った列は途中から周期的になっていそうなので b を勘で見つける
        // a @ b @ b @ b @ b @ ... @ b @ c

        let a = "1,3,2,1,2,1,3,2,2,0,1,3,2,0,2,1".Split(",") |> Array.map int64

        let b =
            "3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1"
                .Split(",")
            |> Array.map int64

        let n = 1_000_000_000_000L
        let q = (n - (int64 a.Length)) / (int64 b.Length)
        let r = (n - (int64 a.Length)) % (int64 b.Length)
        let c = Array.take (int r) b

        assert ((int64 a.Length) + (int64 b.Length) * q + (int64 c.Length) = n)

        (Array.sum a) + (Array.sum b) * q + (Array.sum c) |> should equal 1514285714288L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let jets = parse input

    part1 jets |> printfn "part1: %d"

    0
