[<AutoOpen>]
module Common.FSharp.TestHelpers.TestRunners

open System
open Common.FSharp

/// Run functions with the [<TestSelected>] attribute
let RunAttributeTests attribute (classType: Type) = 
    let todoAssertWasEnabled = TODOAssertEnabled
    TODOAssertEnabled <- false
    use d =
        { new IDisposable with
            member x.Dispose () =
                TODOAssertEnabled <- todoAssertWasEnabled
        }

    let hasCustomAttribute (attribute) (mi: Reflection.MethodInfo) = 
            mi.GetCustomAttributes(attribute, false).Length > 0

    let publicMethods = classType.GetMethods(System.Reflection.BindingFlags.Instance|||System.Reflection.BindingFlags.Public)
        
    let cleanup =
        let cleanupAttributeMethods =
            publicMethods
            |> Array.filter (fun mi -> 
                hasCustomAttribute typeof<TestCleanup> mi
            )
        fun (o: obj) ->
            o |> TryCast<IDisposable> |> Option.iter (dispose)
            cleanupAttributeMethods |> Array.iter (fun mi -> mi.Invoke( o, [||]) |> ignore)

    let instance = classType.GetConstructor([||]).Invoke [||]
    use cleanup = IDisposable.Create (delay cleanup instance)
    
    let classIsSelected =
        attribute = typeof<TestSelected>
        && classType.GetCustomAttributes(typeof<TestSelected>, false).Length > 0
    
    let classIsDebugging =
        attribute = typeof<TestSelected>
        && classType.GetCustomAttributes(typeof<DebugAttribute>, false).Length > 0

    let selected =
        publicMethods
        |> Array.filter (
            if classIsSelected then
                (fun mi -> hasCustomAttribute attribute mi || hasCustomAttribute typeof<TestMethodAttribute> mi)
            else
                hasCustomAttribute attribute
        )

    if selected.Length = 0 then
        printfn "Running <%s> for type <%s>: ... none found" attribute.Name classType.Name

    else
        printfn "-------------------------------------------------"
        printfn "Running <%s> for type <%s>:\n" attribute.Name classType.Name
        let margin = 2
        let padding = 2
        let wrapIndent = 0
        use _indent = Console.InjectIndent margin

        let mutable passed, failed = 0, 0
        for test in selected do
            //
            let wasVerbose = Verbose.Active
            Verbose.Active <-
                wasVerbose
                || hasCustomAttribute typeof<Verbose> test
                || classIsSelected
            use _v = 
                { new IDisposable with
                    member x.Dispose () = Verbose.Active <-wasVerbose
                }
            //
            let wasDebugging = DebugAttribute.Active
            DebugAttribute.Active <-
                wasDebugging
                || hasCustomAttribute typeof<DebugAttribute> test
                || classIsDebugging
            use _v = 
                { new IDisposable with
                    member x.Dispose () = DebugAttribute.Active <-wasDebugging
                }
            
            //
            use _indent = Console.BoxText (Console.WindowWidth - 1 - (margin * 2)) padding wrapIndent
            printfn "Running Test `%s`:" test.Name
            try
                let returnType = test.ReturnType
                let typeofTestWaitForCompletion = typeof<TestWaitForCompletion>
                let result =
                    printfn ""
                    test.Invoke (instance, [||])
                match test, result with
                | (Passes (hasCustomAttribute typeofTestWaitForCompletion)), (:? Coordination.IOnceEvent<unit> as stopped) ->
                    Coordination.OnceEvent.awaitThread stopped
                | _, result ->
                    match result with
                    | :? unit -> ()
                    | :? TestOutput as o ->
                        printfn "Test Output:\n%s" o.Console
                    | o ->
                        printfn "Test Output: %A" o
                printfn "Test Passed."
                passed <- passed + 1
            with e ->
                printfn "Test Failed: %A" e
                failed <- failed + 1

        dispose _indent

        printfn "Results for `%s:`" classType.Name
        printfn " %d Passed. %d Failed. %d Total" passed failed (passed + failed)
        printfn "-------------------------------------------------"
    ()
    
open Microsoft.VisualStudio.TestTools.UnitTesting
type TestClass = TestClassAttribute
type TestMethod = TestMethodAttribute
type TestCleanup = TestCleanupAttribute
type TestInitialize = TestInitializeAttribute

let RunSelectedTests (classType: Type) = RunAttributeTests typeof<TestSelected> classType
let RunManualTests (classType: Type) = RunAttributeTests typeof<TestManually> classType
let RunAllTests (classType: Type) = RunAttributeTests typeof<TestMethodAttribute> classType

let RunClassTests (classAttribute) (methodAttribute) (handle: Type) = 
    let assembly = handle.Assembly
    let selectedTypes = 
        assembly.GetTypes ()
        |> Array.filter (fun ty ->
            ty.GetCustomAttributes (classAttribute, false)
            |> (Array.isEmpty >> not)
        )
    selectedTypes
    |> Array.iter (RunAttributeTests methodAttribute)
    
let RunSelectedClassSelectedTests t = RunClassTests typeof<TestSelected> typeof<TestSelected>        t
let RunSelectedClassAllTests      t = RunClassTests typeof<TestSelected> typeof<TestMethodAttribute> t
