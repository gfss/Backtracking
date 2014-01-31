module Tests.BackTrack.Goals

open Common.FSharp
open System

open BackTrack
open Unification
open Goals

// -------------------------------------
// ---- Tests --------------------------
open Common.FSharp.TestHelpers

module UTests = Tests.BackTrack.Unification
open UTests

let private trimStrs = Array.map (fun (s : string) -> s.Trim ())

let gatom s = Atom (AString s)
let gvar s = LocalVar (Local s)
let gcomp tag args = Compound { Tag = tag; Args = args }
let state l c : State = {
//        Locals      = ( v |> Seq.map (Pair.apply (Local,id)) |> Map.ofSeq)
    Locals      = l
    Constraints = c
}

let parseLocals (s : string) = 
    ((Map.empty : LocalMap), s.Split ';')
    |> Array.foldPair (fun state localAssignment ->
        let parse (local : string, value : string) = 
            let parsedValue = parseConstraint value
            state.Add (Local local, parsedValue)

        match localAssignment.Split '=' |> trimStrs with
        | [| ""; _ |]
        | [| _; "" |] -> failwithf "unable to parse as local assignment: %s" localAssignment
        | [| local; value |] -> parse (local, value)
        | _ -> failwithf "unable to parse as local assignment: %s" localAssignment
    )

let gempty = (Compound { Tag = listTag; Args = [| |] })
let gcons h t = (Compound { Tag = listTag; Args = [| h; t |] })
        
let parseGoalArg = 
    let localRegex = System.Text.RegularExpressions.Regex @"^\$(.+)$"

    let rec parseValue = fun (encoded : string) ->
        match encoded with
        | GivesSome (Text.RegularExpressions.tryMatch localRegex) groups ->
            LocalVar (Local <| groups.[1].Value)
        | GivesSome (tryParseAtom) atom ->
            Atom (atom)
        | GivesSome (tryParseList parseValue gcons gempty) constr ->
            constr
        | _ ->
            failwithf "unknown value <%s>" encoded
    parseValue

let parse1GoalArgs (s : string) = 
    s.Split(',')
    |> trimStrs
    |> Array.map (parseGoalArg)

let parse1ResolveArgs (s : string) = 
    s.Split(',')
    |> trimStrs
    |> Array.map parseConstraint

type private TestOp = 
    | Title of string
    | Resolve of string * string * string * (string * string) option

module Parsers = 
    open System.Collections.Generic

    type SBuilder () = 
        let sb = System.Text.StringBuilder ()
            
        member x.Append (c : char)   = sb.Append c |> ignore
        member x.Append (s : string) = sb.Append s |> ignore

        override x.ToString () = sb.ToString ()
    ()
//        open System.Text.RegularExpressions
    type ParserIterator = 
        abstract Current  : char
        abstract MoveNext : unit -> unit
        abstract Finished : bool

    [<AutoOpen>]
    module Ext = 
        type ParserIterator
        with
            member x.Pop () =
                let c = x.Current
                x.MoveNext ()
                c
                

    type Extractor = 
        abstract Test : ParserIterator -> string option

    let revToString (cs : char list) = String (cs |> Array.ofList |> Array.rev)

    let isLower c = (c >= 'a' && c <= 'z')
    let isUpper c = (c >= 'A' && c <= 'Z')
    let isNum   c = (c >= '0' && c <= '9')
    let isAlpha c = isLower c || isUpper c
    let isAlphaNum c = isLower c || isUpper c || isNum c
    let isNameC c = isLower c || isUpper c || isNum c || c = '_'

    let parseVarName = 
        { new Extractor with
            member x.Test iterator =
                if isUpper iterator.Current
                then
                    let sb = SBuilder ()
                    while isUpper iterator.Current do
                        sb.Append (iterator.Pop ())
                    Some (sb.ToString ())
                else
                    None
        }

    let parseAtom = 
        { new Extractor with
            member x.Test iterator =
                if iterator.Current = '\'' then
                    let sb = SBuilder ()
                    sb.Append (iterator.Pop ())
                    while iterator.Current <> '\'' do
                        sb.Append (iterator.Pop ())
                    sb.Append (iterator.Pop ())
                    Some (sb.ToString ())

                elif iterator.Current = '#' then
                    let sb = SBuilder ()
                    sb.Append (iterator.Pop ())
                    while isNameC iterator.Current do
                        sb.Append (iterator.Pop ())
                    Some (sb.ToString ())
                    
                elif isLower iterator.Current then
                    let sb = SBuilder ()
                    while isNameC iterator.Current do
                        sb.Append (iterator.Pop ())
                    Some (sb.ToString ())

                else
                    None
        }

    let parseVar (prefix) = 
        let nameExtractor = parseVarName
        { new Extractor with
            member x.Test iterator =
                if iterator.Current = prefix
                then
                    iterator.MoveNext ()
                    Some (Option.get <| parseVarName.Test iterator)
                else
                    None
        }

    let parsePtr    = parseVar ('&')
    let parseLocal  = parseVar ('$')

    let prefixList  = '['
    let suffixList  = ']'
    let delimiList  = ':'
    let delimiEq    = '='
    let delimiNeq   = ';'

    let (|Sym|) s =
        let rec skipSpaces s =
            match s with
            | ' ' :: t -> skipSpaces t
            | _ -> s
        skipSpaces s

//        let parseConstraint (Sym input) = 
//            match input with
//            | [] -> None
//            | 
//            
        
    ignore <| Resolve ("X;Y;Z", "($H, $T, [$H:$T])", "(&X, &Y, &Z)", Some ("H=&_G1; T=&_G2", "X;Y;Z=[&_G1:&_G2]; _G1=&X; _G2=&Y"))

let runTests () = 
    printfn "**************************************"
    printfn "Running Predicate tests"

    let tests = System.Collections.Generic.List ()
        
    tests.Add <| Title "Resolve tests"
        
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "#b, #b", Some ("E=#b", "X;Y=#b"))
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "#b, #c", None)
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "#b, &Y", Some ("E=#b", "X;Y=#b"))
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "&Y, #b", Some ("E=&Y", "X;Y=#b"))
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "&X, &Y", Some ("E=&X", "X>Y=#b"))
//        tests.Add <| Resolve ("X;Y=#b", "$E, $E", "&Y, &X", Some ("E=&Y", "X>Y=#b"))
//        
    tests.Add <| Resolve ("X;Y;Z", "$H, $T, [$H:$T]", "&X, &Y, &Z", Some ("H=&_G1;T=&_G2", "X;Y;Z=[&_G1:&_G2];_G1=&X;_G2=&Y"))
    tests.Add <| Resolve ("X;Y;Z", "[$H:$T], $H, $T", "&Z, &X, &Y", Some ("H=&_G1;T=&_G2", "X;Y;Z=[&_G1:&_G2];_G1=&X;_G2=&Y"))
            
    let countPassed = ref 0
    let countTotal  = ref 0
    tests
    |> Seq.iter (function
        | Title title ->
            printfn ""
            printfn "======================================"
            printfn "%s\n" title

        | Resolve (constraintsEncoded, goalsEncoded, argsEncoded, expectedEncoded) ->
            countTotal := !countTotal + 1
            try
                let ostring (r : State option) = (r.Map string).DefaultTo "None"
                let constraints = parseConstraints constraintsEncoded
                let args        = parse1ResolveArgs argsEncoded
                let expected    =
                    expectedEncoded
                    |> Option.map (
                        Pair.apply (parseLocals, parseConstraints)
                        >> uncurry state
                    )
                let goals = parse1GoalArgs goalsEncoded
                printf "{%-10s}  ||  (%-10s)=(%-10s)  =>  {%-20s} ... "
                    <| constraintsEncoded 
                    <| goalsEncoded
                    <| argsEncoded
                    <| expectedEncoded.Map(string).DefaultTo "None"
                let actual      = resolve goals constraints args 
                let pass () = 
                    printfn "Passed"
                    countPassed := !countPassed + 1
                let fail () = 
                    printfn "\nFailed:\nExpected %s \nActual   %s" <| ostring expected <| ostring actual
                    printfn "\n"
                match actual, expected with
                | None, None -> pass ()
                | Some actual, Some expected ->
                    let expected =  
                        { expected with
                            Constraints = { expected.Constraints with N = actual.Constraints.N }
                        }
                    if  expected = actual
                    then pass ()
                    else fail ()
                | _ ->
                    fail ()
            with e ->
                printfn "Exception %A" e
                printfn "\n"
    )
    printfn ""
    printfn "%d/%d Tests passed" !countPassed !countTotal
    ()