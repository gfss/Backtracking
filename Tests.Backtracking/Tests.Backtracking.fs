module Tests.BackTrack.Predicates

open Common.FSharp.TestHelpers
open Common.FSharp

open BackTrack
open Unification
open Goals
open Predicates
    
module GTests = Tests.BackTrack.Goals
module UTests = Tests.BackTrack.Unification
open GTests
open UTests

let conjoinAll goals = 
    match goals with
    | [||] -> None
    | goals ->
        let rec accum i =
            let goal = goals.[i]
            if i + 1 = goals.Length
            then
                Goal goal
            else
                Conj ( (Goal goal), accum (i + 1) )
        Some (accum 0)

let makeRb (preds : Predicate seq) : RuleBase = 
    { Rules = 
        preds
        |> Seq.groupBy (fun pred ->
            PredDefn (pred.Head.Name, pred.Head.Args.Length * 1<args>)
        )
        |> Map.ofSeq
    }

let example rb pred args = 
    printfn ""
    printfn "?- %s(%s)" <| string pred <| (args |> Array.map string |> String.concat ", ")
    solveNew rb pred args
    |> Seq.iter (fun solution ->
        solution.Locals
        |> Seq.map (fun (KeyValue(local, constr)) ->
            sprintf "%s=%s"
                <| string local
                <| string (solution.Constraints.simplifyConstraint constr)
        )
        |> String.concat "; "
        |> function 
            | "" -> "true"
            | s -> s
            |> printfn "\t%s;"
    )
    
let runTests () = 
    printfn "**************************************"
    printfn "Running Backtrackign tests"

    let tests = System.Collections.Generic.List ()
        
    let afact name atoms : Predicate = 
        {
            Head =
                {
                    Name = Predicate name
                    Args = atoms |> Array.map gatom
                }
            Goals = None
            StartPos = 0
            EndPos = 0
        }
    let apred name input (goals) = 
        {
            Head = 
                {
                    Name = name
                    Args = input
                }
            Goals = goals
            StartPos = 0
            EndPos = 0
        }

    let p s = Predicate s

    let familyRb = 
        makeRb
            [|
                afact "childOf" [|"lisa" ; "marge"|]
                afact "childOf" [|"lisa" ; "homer"|]
                afact "childOf" [|"bart" ; "marge"|]
                afact "childOf" [|"bart" ; "homer"|]
                afact "childOf" [|"maggy"; "marge"|]
                afact "childOf" [|"maggy"; "homer"|]
                afact "childOf" [|"homer"; "grandpa"|]
                afact "childOf" [|"marge"; "grandma"|]
                afact "childOf" [|"patty"; "grandma"|]
                afact "childOf" [|"selma"; "grandma"|]
                apred
                    <| Predicate "descendantOf"
                    <| [| gvar "Desc"; gvar "Ansc" |]
                    <| conjoinAll [| { Name = p "childOf"; Args = [| gvar "Desc"; gvar "Ansc" |] } |]

                apred
                    <| Predicate "descendantOf"
                    <| [| gvar "Desc"; gvar "Ansc" |]
                    <| conjoinAll [|
                            { Name = p "childOf"; Args = [| gvar "Desc"; gvar "Parent" |] }
                            { Name = p "descendantOf"; Args = [| gvar "Parent"; gvar "Ansc" |] }
                        |]

                apred
                    <| Predicate "append"
                    <| parse1GoalArgs "[], $E, [$E]"
                    <| conjoinAll [| |]

                apred
                    <| Predicate "append"
                    <| parse1GoalArgs "[$H|$T], $E, [$H|$RT]"
                    <| conjoinAll [| {Name = p "append"; Args = parse1GoalArgs "$T, $E, $RT" } |]

                apred
                    <| Predicate "rev6"
                    <| parse1GoalArgs "$RAcc, $LAcc, [], $LAcc, $RAcc, []"
                    <| conjoinAll [| |]

                apred
                    <| Predicate "rev6"
                    <| parse1GoalArgs "$LO, $RO, [$LH|$LT], $LAcc, $RAcc, [$RH|$RT]"
                    <| conjoinAll [|
                            {Name = p "rev6"; Args = parse1GoalArgs "$LO, $RO, $LT, [$LH|$LAcc], [$RH|$RAcc], $RT" }
                        |]

                apred
                    <| Predicate "rev"
                    <| parse1GoalArgs "$L, $R"
                    <| conjoinAll [|
                            {Name = p "rev6"; Args = parse1GoalArgs "$L, $R, $L, [], [], $R" }
                        |]
            |]
    //
    familyRb.Rules
    |> Map.iter (fun k v ->
        v |> Seq.iter (string >> printfn "%s\n")
    )
            
    example familyRb (p "rev") (parse1GoalArgs "[], []")
    example familyRb (p "rev") (parse1GoalArgs "[#1:#2], [#2:#1]")
    example familyRb (p "rev") (parse1GoalArgs "[#1:#2], $B")
    example familyRb (p "rev") (parse1GoalArgs "$A, [#2:#1]")
//        example familyRb (p "revAcc") (parseGoalArgs "[], []")
//        example familyRb (p "revAcc") (parseGoalArgs "[#1:#2], [#2:#1]")
//        example familyRb (p "revAcc") (parseGoalArgs "[#1:#2], $B")
//        example familyRb (p "revAcc") (parseGoalArgs "$A, [#2:#1]")
//        example familyRb (p "append") (parseGoalArgs "[#1], #2, $R")
//        example familyRb (p "append") (parseGoalArgs "[#1:$A:#3], #4, $R")
//        example familyRb (p "append") (parseGoalArgs "[#1:$A:#3], #4, [#1:#2:#3:#4]")
//        example familyRb (p "append") (parseGoalArgs "[#1:$A:#3], #4, [#1:#2:#3:#3]")
//        example familyRb (p "append") (parseGoalArgs "$Front, #4, [#1:#2:#3:#4]")
//        example familyRb (p "append") (parseGoalArgs "$Front, $Back, [#1:#2:#3:#4]")
//        example familyRb (p "childOf") [| gvar "Desc"; gatom "grandma" |]
//        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "lisa"    |]
//        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "marge"   |]
//        example familyRb (p "descendantOf") [| gvar "Desc"; gatom "grandma" |]
//        example familyRb (p "descendantOf") [| gatom "lisa"    ; gvar "Ansc" |]
//        example familyRb (p "descendantOf") [| gatom "marge"   ; gvar "Ansc" |]
//        example familyRb (p "descendantOf") [| gatom "grandma" ; gvar "Ansc" |]
//        example familyRb (p "descendantOf") [| gvar "Desc" ; gvar "Ansc" |]
    ()