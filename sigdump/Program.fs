// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Mono.Cecil
open Sigdump

[<EntryPoint>]
let main argv = 
    if argv.Length < 1 then
        printfn "usage: sigdump assemblyPath [-h] [-o outputFile]"
    else
        let h = argv |> Array.exists (fun a -> a = "-h" || a = "--hash")
        let o = argv |> Array.tryFindIndex (fun a -> a = "-o" || a = "--output")
        let c = not h

        let o =
            match o with
                | Some i when i < argv.Length - 1 -> Some argv.[i+1]
                | _ -> None

        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let content =
            if h then Sigdump.computeHash argv.[0]
            else Sigdump.toString argv.[0]

        sw.Stop()

        match o with
            | Some _ -> printfn "took: %.2fms" sw.Elapsed.TotalMilliseconds
            | _ -> ()

        match o with
            | None -> printfn "%s" content
            | Some f -> System.IO.File.WriteAllText(f, content)

    0 // return an integer exit code
