namespace LangTests

open ProjectParser
open ProjectInterpreter
open FnuPlot
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ParseTest () =

    [<TestMethod>]
    member this.ValidExprSeqReturnsAnExprList () =
        let input = ["let x = (sin(200t) + 440Hz)"; "play x"; "draw x"]
        let expected = ExprList [AssignOp("x", ChordVal(Notes [Sine 200. ; Hz 440. ])); PlayOp(Variable "x" ); DrawOp(Variable "x" )]
        let result = parse input
        match result with
        |Some x -> Assert.AreEqual(expected, x)
        |None -> Assert.IsTrue(false)
    [<TestMethod>]
    member this.ValidIntOpsReturnsOps () =
        let input = ["A/2 + B*3"]
        let expected = ExprList [AddOp(DivideOp(ChordVal(SingleNote(Key "A")), 2.), MultOp(ChordVal(SingleNote(Key "B")), 3.))]
        let result = parse input
        match result with
        |Some x -> Assert.AreEqual(expected, x)
        |None -> Assert.IsTrue(false)
    [<TestMethod>]
    member this.EmptyReturnsFailure () =
        let input = []
        let result = parse input
        match result with
        |Some x -> Assert.IsTrue(false)
        |None -> Assert.IsTrue(true)
    [<TestMethod>]
    member this.ValidPrintOpReturnsPrintOp () =
        let input = "print A"
        let expected = PrintOp(ChordVal(SingleNote(Key "A")))
        let result = parserepl input
        match result with
        |Some x -> Assert.AreEqual(expected, x)
        |None -> Assert.IsTrue(false)

[<TestClass>]
type InterpretTest () =
    [<TestMethod>]
    member this.ValidVariableReturnVarAndCtx () =
        let gp = new GnuPlot()
        let inputVar = Variable "x"
        let inputCtx = Map.ofList [("x", ChordVal(SingleNote(Hz 220.)))]
        let expected = SingleNote(Hz 220.)
        let (result, ctx) = eval inputVar (inputCtx,gp)
        Assert.AreEqual(expected, result)
    [<TestMethod>]
    member this.ValidAssignOpAddsToContext () =
        let gp = new GnuPlot()
        let inputVar = AssignOp("x", ChordVal(Notes [Key "A"; Key "E"]))
        let inputCtx = Map.ofList []
        let expected = Map.ofList [("x", ChordVal(Notes [Key "A"; Key "E"]))]
        let (e, (result,gp)) = eval inputVar (inputCtx,gp)
        Assert.AreEqual(expected, result)
