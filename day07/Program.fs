open Xunit
open FsUnit.Xunit

type File = { Path: string list; Size: int }

let parse (input: string seq) =
    let rec p (input: string list) cwd files =
        match input with
        | [] -> files
        | "$ cd .." :: rest -> p rest (List.tail cwd) files
        | cd :: rest when cd.StartsWith("$ cd ") ->
            let dir = cd.Replace("$ cd ", "")
            p rest (dir :: cwd) files
        | "$ ls" :: rest ->
            let commandOutput (s: string) = not (s.StartsWith("$"))
            let outputs = rest |> List.takeWhile commandOutput
            let rest = rest |> List.skipWhile commandOutput

            let files' =
                outputs
                |> List.filter (fun s -> not (s.StartsWith("dir")))
                |> List.map (fun s ->
                    match s.Split(" ") with
                    | [| size; name |] ->
                        { Path = (List.rev (name :: cwd))
                          Size = int size }
                    | _ -> failwith $"s = {s}")

            p rest cwd (files @ files')
        | _ -> failwithf "input = %A" input

    let input = List.ofSeq input
    assert (List.head input = "$ cd /")
    p (List.tail input) [] []

let rec solve (files: File seq) =
    let fileSizeTotal =
        files |> Seq.sumBy (fun f -> if List.length f.Path = 1 then f.Size else 0)

    let files =
        files
        |> Seq.filter (fun f -> List.length f.Path >= 2)
        |> Seq.groupBy (fun f -> List.head f.Path) // 同じディレクトリのファイルたちをまとめる

    let dirSizeTotal, allSubDirSizes =
        ((0, []), files)
        ||> Seq.fold (fun (dirSizeTotal, allSubDirSizes) (_, files) ->
            let dirSizeTotal', allSubDirSizes' =
                solve (Seq.map (fun f -> { f with Path = List.tail f.Path }) files)

            dirSizeTotal + dirSizeTotal', allSubDirSizes @ allSubDirSizes')

    let total = fileSizeTotal + dirSizeTotal
    total, total :: allSubDirSizes

let part1 files =
    files |> solve |> snd |> Seq.filter (fun size -> size <= 100000) |> Seq.sum

let part2 files =
    let rootSize, sizes = solve files
    sizes |> Seq.filter (fun s -> 70000000 - (rootSize - s) >= 30000000) |> Seq.min


module Example =
    let input =
        [ "$ cd /"
          "$ ls"
          "dir a"
          "14848514 b.txt"
          "8504156 c.dat"
          "dir d"
          "$ cd a"
          "$ ls"
          "dir e"
          "29116 f"
          "2557 g"
          "62596 h.lst"
          "$ cd e"
          "$ ls"
          "584 i"
          "$ cd .."
          "$ cd .."
          "$ cd d"
          "$ ls"
          "4060174 j"
          "8033020 d.log"
          "5626152 d.ext"
          "7214296 k" ]

    let files = parse input

    [<Fact>]
    let testPart1 () = part1 files |> should equal 95437

    let testPart2 () = part2 files |> should equal 24933642


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd().Split("\n")
    let files = parse input

    part1 files |> printfn "part1: %d"
    part2 files |> printfn "part2: %d"

    0
