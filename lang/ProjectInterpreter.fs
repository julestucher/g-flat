module ProjectInterpreter

open ProjectParser
open System.IO
open FnuPlot
open WAVLibrary

type Context = Map<string,Expr> * GnuPlot

let draw (c: Chord, gp: GnuPlot) =
    let sinstr i = "sin(" + i.ToString() + "*x)"
    let sinnote (n: Note) =
        match n with
        |Hz(i) -> sinstr i
        |Sine(i) -> sinstr i
        |Key(c) ->
            match (mapNotes.TryFind c) with
            |Some i -> sinstr i
            |None -> 
                printfn "%s" "Not a valid note"
                exit(1)

    let rec sinchord (c: Chord) =
        match c with
        |SingleNote(n) -> sinnote n
        |Notes(n::[]) -> sinnote n
        |Notes(n::c') -> (sinnote n) + " + " + (sinchord (Notes(c')))
        |Notes([]) -> ""

    gp.Plot(sinchord c)
    gp

let play (c: Chord) =
    let sinfun h = (fun i -> ((sin ((float i/float 5.78) * (float h/220.))) + 1.)/2. * 255.)
    let rec f c =
        match c with
        |SingleNote(n) ->
            match n with
            |Hz(h) -> sinfun h
            |Sine(h) -> sinfun h
            |Key(c) ->
                match (mapNotes.TryFind c) with
                |Some x -> sinfun x
                |None -> 
                    printfn "Not a valid note"
                    exit(1)
        |Notes(n::[]) -> 
            match n with
            |Hz(h) -> sinfun h
            |Sine(h) -> sinfun h
            |Key(c) ->
                match (mapNotes.TryFind c) with
                |Some x -> sinfun x
                |None -> 
                    printfn "Not a valid note"
                    exit(1)
        |Notes(n::l) ->
            match n with
            |Hz(h) -> (fun i -> (f (Notes(l)) i) + (sinfun h i))
            |Sine(h) -> (fun i -> (f (Notes(l)) i) + (sinfun h i))
            |Key(c) ->
                match (mapNotes.TryFind c) with
                |Some h -> (fun i -> (f (Notes(l)) i) + (sinfun h i))
                |None -> 
                    printfn "not a valid note"
                    exit(1)
        |Notes([]) -> id 
    let g c = 
        match c with
        |SingleNote(n) -> (f c) 
        |Notes(l) -> (fun j -> ((f c) ((float) j)) / ((float)(List.length l)))         
    let data = Array.init 32000 (fun i -> (g c) ((float) i) |> byte)
    let strname = (chordstring c) + ".wav"
    let stream = File.Create(strname)
    write stream data
    playWAV strname

let add c1 c2 =
    match c1 with
    |SingleNote(n1) ->
        match c2 with
        |SingleNote(n2) -> Notes([n1; n2])
        |Notes(l2) -> Notes(n1::l2)
    |Notes(l1) ->
        match c2 with
        |SingleNote(n2) -> Notes(n2::l1)
        |Notes(l2) -> Notes(List.append l1 l2)

let rec divide c (d:float) = 
    match c with
    |SingleNote(n) ->
        match n with
        |Hz(h) -> SingleNote(Hz(h/d))
        |Sine(h) -> SingleNote(Sine(h/d))
        |Key(c) ->
            match (mapNotes.TryFind c) with
            |Some x -> SingleNote(Hz(x/d))
            |None -> 
                printfn "Not a valid note"
                exit(1)
    |Notes(l) -> 
        let l' = List.map (fun n -> (divide (SingleNote(n)) d)) l
        let l'' = List.map (fun c' ->
            match c' with 
            |SingleNote(n) -> n
            |Notes(_) -> 
                printfn "Divide error"
                exit(1) ) l'
        Notes(l'')
let rec mult c (d:float) = 
    match c with
    |SingleNote(n) ->
        match n with
        |Hz(h) -> SingleNote(Hz(h*d))
        |Sine(h) -> SingleNote(Sine(h*d))
        |Key(c) ->
            match (mapNotes.TryFind c) with
            |Some x -> SingleNote(Hz(x*d))
            |None -> 
                printfn "Not a valid note"
                exit(1)
    |Notes(l) -> 
        let l' = List.map (fun n -> (mult (SingleNote(n)) d)) l
        let l'' = List.map (fun c' ->
            match c' with 
            |SingleNote(n) -> n
            |Notes(_) -> 
                printfn "Mult error"
                exit(1) ) l'
        Notes(l'')

let rec eval (e: Expr) (ctx: Context) : Chord*Context=
    match e with
    |ChordVal(c) -> c,ctx
    |PlayOp(c) ->
        let (c',ctx1) = eval c ctx
        play c'
        c',ctx1
    |DrawOp(c) ->
        let (c',ctx') = eval c ctx
        let ctx'',g = ctx'
        let (g') = draw(c', g)
        c',(ctx'',g)
    |AddOp(c1, c2) ->
        let (c1',ctx1) = eval c1 ctx
        let (c2',ctx2) = eval c2 ctx1
        eval (ChordVal(add c1' c2')) (ctx2)     
    |DivideOp(c, n) ->
        let (c',ctx1) = eval c ctx
        eval (ChordVal(divide c' n)) ctx1
    |MultOp(c, n) ->
        let (c',ctx1) = eval c ctx
        eval (ChordVal(mult c' n)) ctx1
    |AssignOp(s, e) ->
        let c,ctx1 = eval e ctx
        let map,g = ctx1
        let ctx2 = Map.add s e map
        c, (ctx2,g)
    |Variable(s) ->
        let map,g = ctx
        if (Map.containsKey s map) then 
            eval map.[s] ctx
        else
            printfn "Unassigned varialbe"
            exit(1)
    |PrintOp(e) -> 
        printfn "%s" (prettyprint e)
        SingleNote(Hz(0.)), ctx
    |ExprList(e::[]) -> eval e ctx
    |ExprList(e::L) -> 
        let (c, ctx1) = eval e ctx
        eval (ExprList(L)) ctx1
    |ExprList(_) -> 
        printfn "Empty list"
        exit(1)

