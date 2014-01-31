module Tests.BackTrack.Parsers

open Common.FSharp

open BackTrack

open Unification
open Goals
open Predicates

open System
open System.Collections.Generic

let listTag   = Unification.listTag
let tupleTag  = Unification.tupleTag
let emptyList = GoalArg.Compound (GoalCompound.New listTag [||])

[<AutoOpen>]
module private Impl = 
    ()
    
let isWs    = Char.IsWhiteSpace 
let isLower = Char.IsLower 
let isUpper = Char.IsUpper 
let isNum   = Char.IsDigit 
let isAlpha = Char.IsLetter 
let isAlphaNum = Char.IsLetterOrDigit
let isNameC c = isLower c || isUpper c || isNum c || c = '_'

type IReader = 
    abstract MoveNext   : unit -> unit
    abstract Current    : char
    abstract IsEof      : bool
    abstract Pos        : int

type SBuilder (sb : System.Text.StringBuilder) = 
            
    new () = SBuilder (System.Text.StringBuilder ())

    member x.Append (c : char)   = sb.Append c |> ignore
    member x.Append (s : string) = sb.Append s |> ignore

    static member Create (c : char) =
        let sb = SBuilder ()
        sb.Append c
        sb


    member x.String = sb.ToString ()

    override x.ToString () = sb.ToString ()

let (|Popif|_|) cond k (e : IReader) = 
    let c = e.Current
    if cond c then
        e.MoveNext () |> ignore
        Some (k c)
    else
        None

let (|Popifc|_|) ch (e : IReader) = 
    let c = e.Current
    if ch = c then
        e.MoveNext () |> ignore
        Some ()
    else
        None

let (|Scanif|_|) cond k (e : IReader) = 
    let c = e.Current
    if cond c then
        Some (k c)
    else
        None


let popWhile cond k (e : IReader) = 
    while cond e.Current do
        k e.Current
        e.MoveNext () |> ignore

let spaces (e : IReader) =
    while not e.IsEof &&  isWs e.Current do
        e.MoveNext () |> ignore

let popExpect (f) k (e : IReader) = 
    if f e.Current
    then k e.Current; e.MoveNext () |> ignore
    else failwithf "Expectation failed"

let inline neq a b = a <> b
let inline eq a b = a = b

let fail () = failwithf "Expectation failed"

let tryParseAtom (e : IReader) = 
    match e with
    | Popif (eq '\'') SBuilder.Create sb ->
        popWhile (neq '\'') sb.Append e
        popExpect (eq '\'') sb.Append e
        Some (AString (sb.String))
    | Popif isLower SBuilder.Create sb ->
        popWhile isNameC sb.Append e
        Some (AString (sb.String))
    | Popifc '#' ->
        let sb = SBuilder ()
        popWhile isNum sb.Append e
        Some (ANum (sb.String |> Int32.Parse))
    | _ ->
        None

let tryParseLocal (e : IReader) = 
    match e with
    | Popif isUpper SBuilder.Create sb ->
        popWhile isNameC sb.Append e
        Some <| Local (sb.String)
    | Popifc '$' ->
        let sb = SBuilder ()
        popWhile isNameC sb.Append e
        Some <| Local (sb.String)
    | _ ->
        None

let rec tryParseGoalCompound (e : IReader) = 
    match e with
    | Popif (eq '[') ignore () ->
        spaces e
        match e with
        | Popif (eq ']') ignore () ->
            Some <| emptyList

        | _ ->
            let rec parseListElements () = 
                let head = parseGoalArg e
                spaces e
                match e with
                | Popifc ']'  ->
                    GoalArg.Compound (GoalCompound.New listTag [|head; emptyList|])
                | Popifc ','  ->
                    spaces e
                    let tail = parseListElements ()
                    GoalArg.Compound (GoalCompound.New listTag [|head; tail|])
                | Popifc '|'  ->
                    spaces e
                    let tail = parseGoalArg e
                    popExpect (eq ']') ignore e
                    GoalArg.Compound (GoalCompound.New listTag [|head; tail|])
                | _ -> failwith "expected ']' ',' or '|'"
            
            Some <| parseListElements ()
    | _ ->
        None

and tryParseGoalArg (e : IReader) = 
    match e with
    | GivesSome tryParseAtom atom ->
        Some <| GoalArg.Atom atom
    | GivesSome tryParseLocal local ->
        Some <| GoalArg.LocalVar local
    | GivesSome tryParseGoalCompound compound ->
        Some compound
    | _ -> None

and parseGoalArg (e : IReader) = 
    tryParseGoalArg e
    |> Option.defaultsTof (fun () -> failwith "expected GoalArg")

let parseGoalArgs (e : IReader) =
    let rec parseArgs (acc) = 
        let head = parseGoalArg e
        spaces e
        match e with
        | Popifc ','  ->
            spaces e
            parseArgs (head :: acc)
        | _ ->
            Array.ofList (List.rev (head :: acc))
    parseArgs []

let tryParseGoal (e : IReader) : Goal option = 
    match e with
    | Popif isLower SBuilder.Create sb ->
        popWhile isNameC sb.Append e
        let name, sb = sb.ToString (), ()
        spaces e
        popExpect (eq '(') ignore e
        spaces e
        let args = parseGoalArgs e
        popExpect (eq ')') ignore e
            
        Some {
            Name = Predicate name
            Args = args
        }
    | _ ->
        None

let parseGoal (e : IReader) = 
    tryParseGoal e
    |> Option.defaultsTof (fun () -> failwith "expected Goal")

let tryParsePredicate (e : IReader) = 
    let startPos = e.Pos
    match tryParseGoal e with
    | Some head ->
        spaces e
        match e with
        | Popifc '.' ->
            Some {
                Head  = head
                Goals = None
                StartPos = startPos
                EndPos   = e.Pos
            }
        | Popifc ':' ->
            popExpect (eq '-') ignore e
            spaces e

            let rec tryParseConjs e = 
                match e with
                | Popifc '(' ->
                    spaces e
                    let contents = parseDisjs e
                    popExpect (eq ')') ignore e
                    contents
                    |> Some
                | GivesSome tryParseGoal goal ->
                    let lhs = Clause.Goal goal
                    spaces e
                    match e with
                    | Popif (eq ',') ignore () ->
                        spaces e
                        let rhs = parseConjs e
                        Clause.Conj (lhs, rhs)
                    | _ ->
                        lhs
                    |> Some
                | _ ->
                    None

            and parseConjs e = 
                tryParseConjs e |> Option.defaultsTof (fun () -> failwithf "expected conj goal")
            
            and parseDisjs e = 
                let lhs = parseConjs e
                spaces e
                match e with
                | Popifc ';' ->
                    spaces e
                    let rhs = parseDisjs e
                    Clause.Disj (lhs, rhs)
                | _ ->
                    lhs

            let body = parseDisjs e
            popExpect (eq '.') ignore e
            Some {
                Head  = head
                Goals = Some body
                StartPos = startPos
                EndPos   = e.Pos
            }
        | _ ->
            failwith "expected '.' or ':-'"
    | None ->
        None

let parsePredicate e =
    tryParsePredicate e |> Option.defaultsTof (fun () -> failwithf "Expected predicate")

let parseProgram (e : IReader) = 
    let lst = Vector<_> ()
    let rec doit () = 
        spaces e
        if e.IsEof
        then
            lst.ToArray ()
        else
            lst.Add (parsePredicate e)
            doit ()
    doit ()

module Tests = 
    open BackTrack.Predicates
    open Tests.BackTrack.Predicates

    let startEnum (s : string) =
        let ie = s.GetEnumerator ()
        let _isEof = ref (not <| ie.MoveNext ())
        let _pos = ref 0
        { new IReader with
            member x.MoveNext () = 
                _isEof := not <| ie.MoveNext ()
                _pos := !_pos + 1
            member x.Current = ie.Current
            member x.IsEof = !_isEof
            member x.Pos   = !_pos
        }

    let mkRule1 (head : string) (body : string[]) = 
        {
            Head  = parseGoal (startEnum head)
            Goals =
                body
                |> Array.map (startEnum >> parseGoal)
                |> conjoinAll
            StartPos = 0
            EndPos   = 0
        }
    let mkFact (head : string) = 
        {
            Head  = parseGoal (startEnum head)
            Goals = None
            StartPos = 0
            EndPos   = 0
        }
    let parseProgram = startEnum >> parseProgram
        
    open Tests.BackTrack.Predicates
    open Tests.BackTrack.Goals

    let runTests () = 
        let parsePredicate = startEnum >> parsePredicate
        let parseGoalArgs = flip (+) "." >> startEnum >> parseGoalArgs
        
        let sfamilyRb = 
            "
                childOf(lisa , marg ) .
                childOf(lisa , home ) .
                childOf(bart , marg ) .
                childOf(bart , home ) .
                childOf(magg , marg ) .
                childOf(magg , home ) .
                childOf(home , grpa ) .
                childOf(marg , grma ) .
                childOf(patt , grma ) .
                childOf(selm , grma ) .
                
                grandChildOf(Desc , Ansc ) :-
                    childOf(Desc, Parent),
                    childOf(Parent, Ansc).
        
                descendantOf(Desc, Ansc) :-
                    childOf(Desc, Ansc) .
                descendantOf(Desc, Ansc) :-
                    childOf(Desc, Parent) ,
                    descendantOf(Parent, Ansc) .
                    
                descendantOf1(Desc, Ansc) :-
                    childOf(Desc, Ansc);
                    childOf(Desc, Parent),
                    descendantOf1(Parent, Ansc).
                    
                descendantOf2(Desc, Ansc) :-
                    childOf(Desc, Parent),
                    descendantOf2(Parent, Ansc);
                    childOf(Desc, Ansc).
                    
                disjreln(Desc, Ansc) :-
                    childOf(Desc, Ansc);
                    childOf(Desc, Parent),
                    descendantOf(Parent, Ansc);
                    childOf(Desc, Ansc).

                rev6(RAcc, LAcc, [], LAcc, RAcc, []).
                rev6(LO, RO, [LH|LT], LAcc, RAcc, [RH|RT]):-
                    rev6($LO, $RO, $LT, [$LH|$LAcc], [$RH|$RAcc], $RT).
                rev($L, $R):-
                    rev6($L, $R, $L, [], [], $R).
            "


        let familyRb = 
            sfamilyRb
            |> parseProgram
            |> Tests.BackTrack.Predicates.makeRb
        
        let parseGoalArgs = parseGoalArgs
        let p s = Predicate s
        Tests.BackTrack.Predicates.example familyRb (p "rev") (parseGoalArgs "[], []")
        Tests.BackTrack.Predicates.example familyRb (p "rev") (parseGoalArgs "[#1,#2], [#2,#1]")
        Tests.BackTrack.Predicates.example familyRb (p "rev") (parseGoalArgs "[#1,#2], $B")
        Tests.BackTrack.Predicates.example familyRb (p "rev") (parseGoalArgs "$A, [#2,#1]")
        example familyRb (p "revAcc") (parseGoalArgs "[], []")
        example familyRb (p "revAcc") (parseGoalArgs "[#1,#2], [#2,#1]")
        example familyRb (p "revAcc") (parseGoalArgs "[#1,#2], $B")
        example familyRb (p "revAcc") (parseGoalArgs "$A, [#2,#1]")
        example familyRb (p "append") (parseGoalArgs "[#1], #2, $R")
        example familyRb (p "append") (parseGoalArgs "[#1,$A,#3], #4, $R")
        example familyRb (p "append") (parseGoalArgs "[#1,$A,#3], #4, [#1,#2,#3,#4]")
        example familyRb (p "append") (parseGoalArgs "[#1,$A,#3], #4, [#1,#2,#3,#3]")
        example familyRb (p "append") (parseGoalArgs "$Front, #4, [#1,#2,#3,#4]")
        example familyRb (p "append") (parseGoalArgs "$Front, $Back, [#1,#2,#3,#4]")
        example familyRb (p "childOf")       (parseGoalArgs "Desc, grpa")
        example familyRb (p "grandChildOf")  (parseGoalArgs "Desc, lisa")
        example familyRb (p "grandChildOf")  (parseGoalArgs "lisa, grpa")
        example familyRb (p "grandChildOf")  (parseGoalArgs "Desc, grpa")
        example familyRb (p "grandChildOf")  (parseGoalArgs "lisa, Ansc")
        example familyRb (p "grandChildOf")  (parseGoalArgs "Desc, Ansc")
        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "lisa"    |]
        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "marg"   |]
        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "grpa" |]
        example familyRb (p "descendantOf") [| gatom "lisa"    ; gvar "Ansc" |]
        example familyRb (p "descendantOf") [| gatom "marg"   ; gvar "Ansc" |]
        example familyRb (p "descendantOf") [| gatom "grpa" ; gvar "Ansc" |]
        example familyRb (p "descendantOf") [| gvar "Desc" ; gvar "Ansc" |]