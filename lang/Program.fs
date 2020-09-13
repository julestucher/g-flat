open ProjectParser
open ProjectInterpreter
open FnuPlot
open System
open System.IO
open System.Threading
open System.Runtime.InteropServices


[<EntryPoint>]
let main argv =
    //Set up the GnuPlot window that will be used throughout
    let scale = 0.03
    let duration = 2
    let gp = new GnuPlot()

    let outputType =
        if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then (Qt)
        else (Window)
    gp.Set
        (output = Output(outputType, font = "arial"),
        style = Style(Solid),
        range = RangeX.[ 0. .. 6.28*scale ])

    let rec repl ctx : unit =
        printf "- "
        let input = System.Console.ReadLine()
        if input = "quit" then
            printfn "Goodbye!"
            exit 0
        else
            let asto = parserepl input
            match asto with
            | Some ast ->
                //printfn "%s" (ast.ToString()) 
                let c,ctx1 = eval ast (ctx,gp)
                let map,g = ctx1
                repl (Map.fold (fun acc key value -> Map.add key value acc) ctx map)
            | None     ->
                printfn "Invalid expression."
                repl ctx
    if(Array.isEmpty argv) then
        printfn "Welcome to Gb repl! \nType \"quit\" to exit.\nMake sure that gnuplot is installed with the qt terminal and PATH variable set \nAnd if you want to run Gb with string interpretation of the program\nenter a string with a seqeunce of commands separated by a new line"
        repl Map.empty |> ignore
    else
        printfn "Welcome to Gb! \nMake sure that gnuplot is installed with the qt terminal and PATH variable set"
        let argparse argv =
            if Array.length argv <> 1 then
                printfn "Please put file name in argument"
                exit(1)
            let (s: String) =
                try
                    (argv.[0]: String)
                with
                | :? System.FormatException as e ->
                    printfn "Not a string"
                    exit(1)
            s
        let filepath = argparse argv
        let filelist = File.ReadAllLines(filepath) |> Array.toList

        match (parse filelist) with
        |Some ast ->
            let (c, ctx1) = eval ast (Map.empty,gp)
            printfn "Done!"
        |None -> 
            printfn "%s" "Not a valid expr"
            exit(1)

    gp.Kill()
    Thread.Sleep(1000)
    0
