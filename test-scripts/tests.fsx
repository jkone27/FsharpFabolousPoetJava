#r "nuget: Fli"
open Fli.CE
open Fli

let log (output: string) =
    $"CLI log: {output}" 
    |> printfn "%s"

let runTest suite =
    cli {
        Exec "dotnet"
        Arguments $"run {suite}"
        Output log
    }
    |> Command.execute