module ProjectParser
open Parser

//t is in terms of 2pi radians

type Note =
|Hz of float
|Sine of float
|Key of string

type Chord =
|SingleNote of Note
|Notes of Note list

type Expr =
|ChordVal of Chord
|PlayOp of Expr
|DrawOp of Expr
|AddOp of Expr * Expr
|DivideOp of Expr * float
|MultOp of Expr * float
|Variable of string
|AssignOp of string * Expr
|PrintOp of Expr
|ExprList of Expr list

let mapNotes = Map.ofList [("C", 261.63); ("C#", 277.18); ("Db", 277.18); ("D", 293.66); ("D#", 311.13); ("Eb", 311.13); ("E", 329.63); ("F", 349.23); ("F#", 369.99); ("Gb", 369.99); ("G", 392.00); ("G#", 415.30); ("Ab", 415.30); ("A", 440.00); ("A#", 466.16); ("Bb", 466.16); ("B", 493.88)]
let notallowed = ['/'; ' '; '\n'; ')'; '('; '+'; '*']
let notestring n =
    match n with
    |Hz(i) -> i.ToString() + "Hz"
    |Sine(i) -> "sin" + i.ToString() + "x"
    |Key(c) -> c.ToString()

let rec chordstring c =
    match c with
    |SingleNote(n) -> notestring n
    |Notes([n]) -> notestring n
    |Notes(n::c') -> (notestring n) + "+" + (chordstring (Notes(c')))
    |Notes([]) -> ""
let rec prettyprint e =
    match e with
    |ChordVal(c) -> chordstring c
    |PlayOp(c) -> "play " + (prettyprint c)
    |DrawOp(c) -> "draw " + (prettyprint c)
    |AddOp(c1, c2) -> (prettyprint c1) + " + " + (prettyprint c2)
    |DivideOp(c,n) -> (prettyprint c) + "/" + n.ToString()
    |MultOp(c,n) -> (prettyprint c) + "*" + n.ToString()
    |AssignOp(s, c) -> s + " = " + (prettyprint c)
    |PrintOp(e) -> "print " + (prettyprint e)
    |Variable(s) -> s
    |ExprList([]) -> ""
    |ExprList([e]) -> prettyprint e
    |ExprList(e::l) -> (prettyprint e) + "\n" + (prettyprint (ExprList(l)))

let expr, exprImpl = recparser()
let pnotws = pmany0 (psat (fun c -> (c <> ' '&& c <> '\n' && c <> '/')))
let pallowed (l : char list) = pmany1 (psat (fun c -> not (List.contains c l)))
let allowedvar (l : char list)= pbind (pallowed l) (fun c -> presult (stringify c))
let preparelist(l: string list) : Input =
    if(l = List.empty) then
        ("", true)
    else
        let rec seqconvert (list: string list) =
            (List.fold (fun s1 s2 -> s1 + "\n" + s2) "" list).[1..]
        prepare (seqconvert l + "\n")
let psatstr (l: char list) (f: string -> bool) : Parser<string> =
    pbind (pallowed l) (fun c -> if (f (stringify c)) then presult (stringify c) else pzero)
let hz = pleft (pmany1 pdigit) (pstr "Hz") |>> (fun l -> Hz(float(stringify l)))
let sine = pbetween (pstr "sin(" ) (pstr "t)") (pmany1 pdigit) |>> (fun l -> Sine(float(stringify l)))
let key = psatstr (notallowed) (mapNotes.ContainsKey) |>> (fun c -> Key(c)) //<!> "key"
let note = hz <|> sine <|> key //<!> "note"
let singleNote = note |>> (fun n -> SingleNote(n)) //<!> "singlenote"
let notes = pbetween (pchar '(') (pchar ')') (pseq (note) (pmany1 (pright (pbetweenws (pchar '+')) note)) (fun (n1,n2) -> Notes(n1::n2))) //<!> "notes"
let chord = notes <|> singleNote //<!> "chord"
let chordVal = chord |>> (fun c -> ChordVal(c)) //<!> "chordVal"
let variable = allowedvar notallowed |>> (fun(l) -> Variable(l)) //<!> "var"
let playOp = pright (pstr "play ") expr |>> (fun c -> PlayOp(c)) //<!> "playOp"
let drawOp = pright (pstr "draw ") expr |>> (fun c -> DrawOp(c)) //<!> "drawOp"
let multable = chordVal <|> variable //<!> "multable"
let divideOp = pseq (pleft multable (pstr "/")) (pmany1 pdigit) (fun (e,n) -> DivideOp(e,(float)(stringify n))) //<!> "divideOp"
let multOp = pseq (pleft multable (pstr "*")) (pmany1 pdigit) (fun (e,n) -> MultOp(e,(float)(stringify n))) //<!> "divideOp"
let addable = multOp <|> divideOp <|> chordVal <|> variable  //<!> "addable"
let addOp = pseq addable (pright (pbetweenws (pstr "+")) expr) (fun (a, b) -> AddOp(a, b)) //<!> "addOp"
let assignOp = pseq (pright (pstr "let ") (pleft (allowedvar notallowed) (pbetweenws (pstr "=")))) expr (fun (v, e) -> AssignOp(v, e)) //<!> "assignOp"
let printOp = pright (pstr "print ") expr |>> (fun c -> PrintOp(c))// <!> "printOp"

exprImpl := assignOp <|> playOp <|> drawOp  <|> printOp <|> addOp <|> addable

let grammar = pleft expr (pstr "\n")

let replgrammar = pleft expr peof

let file = pmany1 (pbetweenws grammar) |>> (fun i -> ExprList(i)) //<!> "file"

let parse input : Expr option =
    match file (preparelist input) with
    | Success(e,_) -> Some e
    | Failure -> None

let parserepl input : Expr option =
    match replgrammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None
