module Program

open Xunit
open FsUnit.Xunit

let rec pow x a =
    if a = 0L then 1L else x * pow x (a - 1L)

let decimal n =
    n
    |> Seq.rev
    |> Seq.mapi (fun i c ->
        let y =
            match c with
            | '2' -> 2L
            | '1' -> 1L
            | '0' -> 0L
            | '-' -> -1L
            | '=' -> -2L
            | _ -> failwith $"c = {c}"

        y * (pow 5L i))
    |> Seq.sum

let rec snafu n =
    if n = 0L then
        ""
    else
        let (q, t) =
            match n % 5L with
            | 0L -> (0L, "0")
            | 1L -> (1L, "1")
            | 2L -> (2L, "2")
            | 3L -> (-2L, "=")
            | 4L -> (-1L, "-")
            | _ -> failwith $"n = {n}"

        snafu ((n - q) / 5L) + t

let solve numbers =
    numbers |> Seq.map decimal |> Seq.sum |> snafu

[<Fact>]
let test () =
    let numbers =
        [ "1=-0-2"
          "12111"
          "2=0="
          "21"
          "2=01"
          "111"
          "20012"
          "112"
          "1=-1="
          "1-12"
          "12"
          "1="
          "122" ]

    solve numbers |> should equal "2=-1=0"

[<EntryPoint>]
let main _ =
    let numbers = stdin.ReadToEnd().Trim().Split("\n")
    solve numbers |> printfn "%s"
    0
