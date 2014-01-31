// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// By default script files are not be part of the project build.

// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
(*
#load "../../Common/FSharp/Core.fs"
#load "../../Common/FSharp/Extensions.fs"
#load "Types.fs"
open Sharp.FSharp
open Backtracking

let equals : Rule = {
    Name  = "equals"
    Args  = [| Free  { Name = "X" }; Free { Name = "X" } |]
    Goals = []
}

let descendantOf1 : Rule =
    let descendant = Free { Name = "Descendant" }
    let ancestor = Free { Name = "Ancestor" }
    {
        Name = "descendantOf"
        Args = [| descendant; ancestor |]
        Goals = [ { Rule = "childOf"; Args = [| descendant; ancestor |] } ]
    }

let descendantOf2 : Rule =
    let descendant = Free { Name = "Descendant" }
    let ancestor = Free { Name = "Ancestor" }
    let parent = Free { Name = "Parent" }
    {
        Name = "descendantOf"
        Args = [| descendant; ancestor |]
        Goals =
            [
                { Rule = "childOf"; Args = [| descendant; parent |] }
                { Rule = "descendantOf"; Args = [| parent; ancestor |] }
            ]
    }
    
let bart : Atom = { Value = "bart" }
let lisa : Atom = { Value = "lisa" }
let marge : Atom = { Value = "marge" }
let homer : Atom = { Value = "homer" }
let selma : Atom = { Value = "selma" }
let grandpa : Atom = { Value = "grandpa" }
let grandma : Atom = { Value = "grandma" }

let familyTree : Fact[] = 
    [|
        bart, marge
        bart, homer
        lisa, marge
        lisa, homer
        homer, grandpa
        marge, grandma
        selma, grandma
    |]
    |> Array.map (fun (child, parent) -> 
        {
            Name = "childOf"
            Atoms = [| child; parent |]
        }
    )

let unify (context : Map<string * int, Rule[]>) (goal : Goal) = 
    match context.TryFind (goal.Rule, goal.Args.Length) with
    | Some rules ->
        rules
        |> Seq.choose ( fun rule ->
            if goal.Args.Length = 0
            then
                let acc = Array.zeroCreate goal.Args.Length
                let rec unifyArgsFrom i = 
                    if i = rule.Goals.Length
                    then
                        Some (acc)
                    else
                        match goal.Args.[i], rule.Args.[i] with
                        | Atom goalArg, Atom ruleArg ->
                            if goalArg = ruleArg
                            then
                                acc.[i] <- Atom goalArg
                                unifyArgsFrom (i + 1)
                            else
                                None
                        | _ ->
                            TODO0 None
                unifyArgsFrom 0
            else
                TODO0 None
        )
    | None -> Seq.empty
    
let allRules = 
    seq {
        yield descendantOf1
        yield descendantOf2
        yield! (familyTree |> Array.map Rule.ofFact)
    }
    |> Seq.groupBy (fun rule ->
        (rule.Name, rule.Args.Length)
    )
    |> Map.ofSeq
    |> Map.map (fun k v -> Array.ofSeq v)
    |> Map.map (hconst Array.ofSeq)

let (=?) actual expected = 
    if actual <> expected
    then
        failwithf "Expected (%A). Acual (%A)" expected actual

let test1 () =
    let goal : Goal = {
        Rule = "childOf"
        Args = [|Atom lisa; Atom homer|]
    }
    Array.map Array.ofSeq (Array.ofSeq (unify allRules goal)) =? [| [|Atom lisa; Atom homer|] |]
    ()

let test2 () =
    let goal : Goal = {
        Rule = "childOf"
        Args = [|Atom lisa; Free { Name = "X" }|]
    }
    ()

test1 ()

(*
type UnifyVar = 
    | Unified   of GoalArg
    | Ununified

type UnifyState = Map<Free,UnifyVar>

let unifyVarsFormat (map : UnifyState) =
    map
    |> Seq.map (fun (KeyValue(free,u)) ->
        sprintf "%s = %s"
            <| free.Name
            <| match u with
                | Ununified -> "?"
                | Unified v ->
                    match v with
                    | Free v -> v.Name
                    | Atom v -> v.Value
    )
    |> String.concat "; "

let vars =
    [| "A"; "B"; "C"; "D"; "E"; "F" |]
    |> Array.map (fun s -> { Free.Name = s })
    
let inputVars =
    vars
    |> Array.map (fun v -> v, Ununified)
    |> Map.ofArray
  
  
let tryUnify (var : Free) (unifyTo : GoalArg) (state : UnifyState) : UnifyState option = 
    let rec loop (chain : Free list) (var : Free) =
        match Map.tryFind var state with
        | None ->
            Some (Map.add var (Unified unifyTo) state)
        | Some oldVal ->
            match oldVal with
            | Ununified ->
                Some (Map.add var (Unified unifyTo) state)
            | Unified (oldVal) ->
                match oldVal with
                | Atom oldAtom ->
                    match unifyTo with
                    | Atom newAtom ->
                        if oldAtom = newAtom
                        then
                            Some state
                        else
                            None
                    | Free oldFree ->
                        loop (var :: chain) oldFree
                | Free oldFree ->
                    loop (var :: chain) oldFree
    loop [] var
*)
(*
        [ A = ?; B = ?; C = A; D = ? ]
C = B   [ A = ?; B = A; C = A; D = ? ]
*)
(*
        [ A = ?; B = ?; C = ?; D = ?; E = ?; F = ? ]
B = E   [ A = ?; B = ?; C = ?; D = ?; E = B; F = ? ]
E = F   [ A = ?; B = ?; C = ?; D = ?; E = B; F = E ]
F.      [ A = ?; B = ?; C = ?; D = ?; E = B; F = B ]
A = B   [ A = ?; B = A; C = ?; D = ?; E = B; F = B ]
D = C   [ A = ?; B = A; C = ?; D = C; E = B; F = B ]
A = D   [ A = ?; B = A; C = A; D = A; E = B; F = B ]
C = n   [ A = n; B = A; C = n; D = A; E = B; F = B ]
E.      [ A = n; B = n; C = n; D = A; E = n; F = B ]
*)


//let unify (goals : Goal list) = 
//    
//    ()

// Define your library scripting code here

*)