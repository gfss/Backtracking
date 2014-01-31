module Tests.BackTrack.Unification

open Common.FSharp
open System

open Common.FSharp.TestHelpers

open BackTrack.Unification

// ---------

let private trimStrs = Array.map (fun (s : string) -> s.Trim ())

let listTag = CompoundFormatters.Register (uncurry (sprintf "[%s|%s]"))
do CompoundFormatters.Overload (listTag, "[]")

let tupleTag = CompoundFormatters.Register (String.concat ", " >> sprintf "(%s)" )

let private empty       = Comp (Compound { Tag = listTag; Args = [| |] })
let private cons h t    = Comp (Compound { Tag = listTag; Args = [| h; t |] })
    
let atomRegex = System.Text.RegularExpressions.Regex @"^\#(.+)$"
let listRegex = System.Text.RegularExpressions.Regex @"^\[(.*)\]$"

let tryParseAtom (encoded : string) = 
    match encoded with
    | GivesSome (Text.RegularExpressions.tryMatch atomRegex) groups ->
        Some (AString <| groups.[1].Value)
    | _ ->
        None

let tryParseList<'elem> (parseValue : string -> 'elem) cons empty (encoded : string) = 
    match encoded with
    | GivesSome (Text.RegularExpressions.tryMatch listRegex) groups ->
        let parseHeadElems (head : string) = 
            head.Split ':'
            |> trimStrs
            |> function | [|""|] -> [||] | a -> Array.map (parseValue) a
        match groups.[1].Value.Split '|' |> trimStrs with
        | [| head |] ->
            let elems = parseHeadElems head
            Array.foldBack cons elems empty
            |> Some
        | [| head; tail |] ->
            let headElems = parseHeadElems head
            let tailConstraint = parseValue tail
            Array.foldBack cons headElems tailConstraint
            |> Some
        | _ ->
            failwithf "can't parse list: two tails in <%s>" encoded
    | _ ->
        None

let parseConstraint =
    let pointRegex = System.Text.RegularExpressions.Regex @"^\&(.+)$"

    let rec parseValue = fun (encoded : string) ->
        match encoded with
        | GivesSome (Text.RegularExpressions.tryMatch pointRegex) groups ->
            Equals (Pointer <| groups.[1].Value)
        | GivesSome (tryParseAtom) atom ->
            Comp (Atom atom)
        | GivesSome (tryParseList parseValue cons empty) constr ->
            constr
        | _ ->
            failwithf "unknown value <%s>" encoded
    parseValue

let parseConstraints (s: string) =
    Constraints.Create (
        ( (Map.empty : ConstraintMap), s.Split ';')
        |> Array.foldPair (fun state ring ->
            let parse (pointers : string, value : string option) =
                let parsedValue = value.Map parseConstraint

                let pointers = pointers.Split '>' |> Array.map Pointer
                ((state, 1), pointers)
                |> Array.foldPair (fun (state, nexti) p ->
                    if nexti < pointers.Length
                    then
                        state.Add (p, Some (Equals pointers.[nexti])), nexti + 1
                    else
                        state.Add (p, parsedValue), nexti + 1
                )
                |> fst

            match ring.Split '=' |> Array.map (fun s -> s.Trim()) with
            | [| pointers |]        -> parse (pointers, None)
            | [| pointers; "" |]    -> parse (pointers, None)
            | [| pointers; value |] -> parse (pointers, Some (value))
            | _ -> failwithf "unable to parse as ring: %s" ring
        )
    )
        

type private TestOp = 
    | Title of string
    | Parses of string * Constraints option
    | Unify  of string * Constraint * Constraint * string option

//
let compact (constraints : Constraints) = 
    { constraints with
        Map = 
            constraints.Map
            |> Map.map (fun pointer -> 
                function
                | Some (Equals p) ->
                    let p, _ = constraints.deref p
                    Some (Equals p)
                | v -> v
            )
    }
    
[<AutoOpen>]
module Constructors =
    let point s = Equals (Pointer s)
    let astring s = Comp (Atom (AString s))
    let clist c = c |> flip (List.foldBack cons) empty
    let alist s = s |> List.map astring |> clist
    let constraints v = 
        Constraints.Create ( v |> Seq.map (Pair.apply (Pointer,id)) |> Map.ofSeq)

//
let runTests () = 
    printfn "**************************************"
    printfn "Running Unification tests"

    let tests = System.Collections.Generic.List ()
    
    tests.Add (Title "Parser")
    tests.Add (Parses ( "V;W;X", Some(constraints [ ("V",None); ("W",None); ("X",None) ]) ))
    tests.Add (Parses ( " V=; W= ;X=", Some(constraints [ ("V",None); ("W",None); ("X",None) ]) ))
    tests.Add (Parses ( " V; W=#a ; X = #b ", Some(constraints [ ("V",None); ("W",Some(astring "a")); ("X",Some(astring "b")) ]) ))
    tests.Add (Parses ( "V=&W;W>X=#b", Some(constraints [ ("V",Some(point "W")); ("W",Some(point "X")); ("X",Some(astring "b")) ]) ))
    tests.Add (Parses ( "V = &W ;W>X=#b", Some(constraints [ ("V",Some(point "W")); ("W",Some(point "X")); ("X",Some(astring "b")) ]) ))
    tests.Add (Parses ( "V>W>X=#b", Some(constraints [ ("V",Some(point "W")); ("W",Some(point "X")); ("X",Some(astring "b")) ]) ))
    tests.Add (Parses ( "V;W=[#1,#2]",     Some(constraints [ ("W",Some(alist [ "1"; "2" ])) ; ("V",None) ]) ))
    tests.Add (Parses ( "V;W=[ #1 , #2 ]", Some(constraints [ ("W",Some(alist [ "1"; "2" ])) ; ("V",None) ]) ))
    tests.Add (Parses ( "V;W=[#1,#2,&V]", Some(constraints [ ("W",Some(clist [ astring "1"; astring "2"; point "V" ])) ; ("V",None) ]) ))

    tests.Add (Title "Free Pointer-Pointer Unification")
    tests.Add (Unify ("V;W;X;Y;Z", point "W", point "X", Some "V;W>X;Y;Z"))
    tests.Add (Unify ("V;W>X;Y;Z", point "Y", point "Z", Some "V;W>X;Y>Z"))
    tests.Add (Unify ("V;W>X;Y>Z", point "V", point "W", Some "V>W>X;Y>Z"))
    tests.Add (Unify ("V;W>X;Y>Z", point "W", point "X", Some "V;W>X;Y>Z"))
    tests.Add (Unify ("V;W>X;Y>Z", point "X", point "W", Some "V;W>X;Y>Z"))
    tests.Add (Unify ("V;W>X;Y>Z", point "X", point "Y", Some "V;W>X>Y>Z"))
    tests.Add (Unify ("V;W>X;Y>Z", point "Y", point "X", Some "V;Y>Z>W>X"))
    tests.Add (Unify ("V>W>X;Y>Z", point "Y", point "V", Some "Y>Z>V>W>X"))
    tests.Add (Unify ("V>W>X;Y>Z", point "V", point "Y", Some "V>W>X>Y>Z"))
    
    tests.Add (Title "Pointer-Atom Unification")
    tests.Add (Unify ("V>W>X=  ;Y>Z", point "X", astring "b", Some "V>W>X=#b;Y>Z"))
    tests.Add (Unify ("V>W>X=  ;Y>Z", point "V", astring "b", Some "V>W>X=#b;Y>Z"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "V", astring "b", None))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "V", astring "a", Some "V>W>X=#a;Y>Z"))
    tests.Add (Unify ("V>W>X=  ;Y>Z", astring "b", point "X", Some "V>W>X=#b;Y>Z"))
    tests.Add (Unify ("V>W>X=  ;Y>Z", astring "b", point "V", Some "V>W>X=#b;Y>Z"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", astring "b", point "V", None))
    tests.Add (Unify ("V>W>X=#a;Y>Z", astring "a", point "V", Some "V>W>X=#a;Y>Z"))
    
    tests.Add (Title "Atom Constrained Pointer Unification")
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "V", point "Y", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "V", point "Z", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "X", point "Z", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "Y", point "V", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "Z", point "V", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z", point "Z", point "X", Some "Y>Z>V>W>X=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#a", point "V", point "Y", Some "V>W>X=#a;Y>Z=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#a", point "V", point "Z", Some "V>W>X=#a;Y>Z=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#a", point "X", point "Z", Some "V>W>X=#a;Y>Z=#a"))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#b", point "V", point "Y", None))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#b", point "V", point "Z", None))
    tests.Add (Unify ("V>W>X=#a;Y>Z=#b", point "X", point "Z", None))
    
    tests.Add (Title "List Unification")
    tests.Add (Unify ("V;W=[#1:#2]", point "W", parseConstraint "[#1:#2]", Some "V;W=[#1:#2]"))
    tests.Add (Unify ("V;W=[#1:#2]", point "V", point "W", Some "V>W=[#1:#2]"))
    tests.Add (Unify ("V=[#1:#2];W=[#1:#2]", point "V", point "W", Some "V=[#1:#2];W=[#1:#2]"))
    tests.Add (Unify ("V=[#1:#2:#3];W=[#1:#2]", point "V", point "W", None))
    tests.Add (Unify ("V=[#1:#3];W=[#1:#2]", point "V", point "W", None))
    tests.Add (Unify ("V=[#1:#3];W=[#1:#2]", point "V", point "W", None))
    tests.Add (Unify ("V=[&X:#2];W=[#1:&Y];X;Y",    point "V", point "W", Some "V=[&X:#2];W=[#1:&Y];X=#1;Y=#2"))
    tests.Add (Unify ("V=[&X:#2];W=[#1:&Y];X=#1;Y", point "V", point "W", Some "V=[&X:#2];W=[#1:&Y];X=#1;Y=#2"))
    tests.Add (Unify ("V=[&X:#2];W=[#1:&Y];X=#3;Y", point "V", point "W", None))
    tests.Add (Unify ("V=[#1:&X];W=[#1:&Y];X;Y", point "V", point "W", Some "V=[#1:&X];W=[#1:&Y];X>Y"))

    let countPassed = ref 0
    let countTotal  = ref 0
    tests
    |> Seq.iter (function
        | Title title ->
            printfn ""
            printfn "======================================"
            printfn "%s\n" title

        | Parses (encoded, expected) ->
            countTotal := !countTotal + 1
            try
                let sExpected = expected.Map(string).DefaultTo "None"
                printf "parse %-20s  ?=  %-30s ... " <| sprintf "\"%s\"" encoded <| sExpected
                let actual = parseConstraints encoded
                if Some actual = expected
                then
                    printfn "Passed"
                    countPassed := !countPassed + 1
                else
                    printfn "\nFailed:\nExpected %s \nActual   %s" <| sExpected <| string actual

            with e ->
                if expected = None
                then
                    printfn "passed"
                    countPassed := !countPassed + 1
                else
                    printfn "Exception %A" e

        | Unify (encodedInitialState, c1, c2, encodedExpected) ->
            countTotal := !countTotal + 1
            try
                let ostring r = (Option.map string r).DefaultTo "None"
                let initialState    = parseConstraints encodedInitialState
                let expected        = Option.map parseConstraints encodedExpected
                let actual = unify c1 c2 initialState
                printf "{%-30s}  %-18s  {%-30s} ... "
                    <| encodedInitialState 
                    <| sprintf "%s=%s" (string c1) (string c2)
                    <| (encodedExpected.DefaultTo "")
                let (expected, actual) = (expected, actual) |> Pair.map (Option.map (compact))
    //            actual =? expected
                if actual = expected
                then
                    printfn "Passed"
                    countPassed := !countPassed + 1
                else
                    printfn "\nFailed:\nExpected %s \nActual   %s" <| ostring expected <| ostring actual
                    printfn "\n"
//                    printfn "%s" <| string actual
            with e ->
                printfn "Exception %A" e
                printfn "\n"
    )
    printfn ""
    printfn "%d/%d Tests passed" !countPassed !countTotal

() 