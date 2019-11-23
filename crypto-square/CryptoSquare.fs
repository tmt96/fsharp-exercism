module CryptoSquare

open System
open System.Text.RegularExpressions

let ciphertext (input: string): string =
    let input = Regex.Replace(input, @"[^a-zA-z0-9]", "").ToLower()
    let len = input.Length
    if len = 0 then
        ""
    else
        let col = int (ceil (sqrt (float len)))
        let row = (len - 1) / col + 1

        let getChar i j =
            let pos = j * col + i
            if pos < len then input.[pos]
            else ' '

        Seq.init col (fun i -> Seq.init row (getChar i) |> String.Concat) |> String.concat " "
