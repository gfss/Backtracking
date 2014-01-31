[<AutoOpen>]
module Common.FSharp.TestHelpers.Tests

open System

let inline (=?) actual expected = 
    if actual = expected
    then ()
    else
        let sexpeted = sprintf "%A" expected
        let sactual  = sprintf "%A" actual
        let msg =
            if sexpeted.Length + sactual.Length < 60 then
                sprintf "Expected (%s) = Actual (%s)" sexpeted sactual
            else
                sprintf "Expected: \n    %s\n\nActual: \n    %s" sexpeted sactual
        System.Console.WriteLine msg
        failwith msg


let inline (<>?) a b = 
    if a <> b
    then ()
    else
        let msg = sprintf "Expected (%A) <> (%A)" a b
        System.Console.WriteLine msg
        failwith msg

let inline (<?) a b = 
    if a < b
    then ()
    else
        let msg = sprintf "Expected (%A) < (%A)" a b
        System.Console.WriteLine msg
        failwith msg

let inline (<=?) a b = 
    if a <= b
    then ()
    else
        let msg = sprintf "Expected (%A) <= (%A)" a b
        System.Console.WriteLine msg
        failwith msg

let inline (>?) a b = 
    if a > b
    then ()
    else
        let msg = sprintf "Expected (%A) > (%A)" a b
        System.Console.WriteLine msg
        failwith msg

let inline (>=?) a b = 
    if a >= b
    then ()
    else
        let msg = sprintf "Expected (%A) >= (%A)" a b
        System.Console.WriteLine msg
        failwith msg
        
// -----------------------------------
type TestMethodAttribute = Microsoft.VisualStudio.TestTools.UnitTesting.TestMethodAttribute

///
[<Sealed>]
[<System.AttributeUsageAttribute(System.AttributeTargets.Method|||System.AttributeTargets.Class, AllowMultiple=false, Inherited=false)>]
type TestSelected () =
    inherit System.Attribute ()

///
[<Sealed>]
[<System.AttributeUsageAttribute(System.AttributeTargets.Method, AllowMultiple=false, Inherited=false)>]
type TestCleanup () =
    inherit System.Attribute ()

///
[<System.AttributeUsageAttribute(System.AttributeTargets.Method, AllowMultiple=false, Inherited=false)>]
[<Sealed>]
type TestWaitForCompletion () =
    inherit System.Attribute ()

///
[<System.AttributeUsageAttribute(System.AttributeTargets.Method, AllowMultiple=false, Inherited=false)>]
[<Sealed>]
type TestManually () =
    inherit System.Attribute ()

///
[<System.AttributeUsageAttribute(System.AttributeTargets.Method, AllowMultiple=false, Inherited=false)>]
[<Sealed>]
type Verbose () =
    inherit System.Attribute ()

    static member val Active = false with get, set

///
[<System.AttributeUsageAttribute(System.AttributeTargets.Method, AllowMultiple=false, Inherited=false)>]
[<Sealed>]
type DebugAttribute () =
    inherit System.Attribute ()

    static member val Active = false with get, set
    
let vprintf fmt  = Printf.kprintf (fun s -> if Verbose.Active then System.Console.Write     s) fmt
let vprintfn fmt = Printf.kprintf (fun s -> if Verbose.Active then System.Console.WriteLine s) fmt
let dprintfn fmt = Printf.kprintf (fun s -> if DebugAttribute.Active then System.Console.WriteLine s) fmt

type TestOutput =
    abstract Console : string
