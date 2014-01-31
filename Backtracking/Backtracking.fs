module BackTrack.Predicates

open Unification
open Goals

type PredicateName = Predicate of string
with
    member x.N = 
        match x with
        | Predicate s -> s
    override x.ToString () =
        x.N

type Goal = {
    Name : PredicateName
    Args : GoalArg[]
}
with
    override goal.ToString () =  
        sprintf "%s( %s )" <| goal.Name.N <| (goal.Args |> Array.map string |> String.concat ", ")

type Clause = 
    | Goal of Goal
    | Conj of Clause * Clause
    | Disj of Clause * Clause
with
    member x.BuildString (indent) (sb : System.Text.StringBuilder) = 
        match x with
        | Goal goal ->
            ignore <| sb.Append (string goal)
        | Conj (left, right) ->
            ignore <| left.BuildString (indent) sb
            ignore <| sb.Append (",\n")
            ignore <| sb.Append (' ', indent)
            ignore <| right.BuildString (indent) sb
        | Disj (left, right) ->
            let newIndent = indent + 2
            ignore <| sb.Append ("(\n") 
            ignore <| sb.Append (' ', newIndent)
            ignore <| left.BuildString (newIndent) sb
            ignore <| sb.Append ("\n")
            ignore <| sb.Append (' ', indent)
            ignore <| sb.Append ("); (\n")
            ignore <| sb.Append (' ', newIndent)
            ignore <| right.BuildString (newIndent) sb
            ignore <| sb.Append ("\n")
            ignore <| sb.Append (' ', indent)
            ignore <| sb.Append (")")
        sb

    override x.ToString () =
        let sb = System.Text.StringBuilder ()
        ignore <| x.BuildString 4 sb
        string sb

type Predicate = {
    Head  : Goal
    Goals : Clause option
    StartPos    : int
    EndPos      : int

}
with
    override pred.ToString () =     
        sprintf "%s%s."
            <| string pred.Head
            <| (
                match pred.Goals with
                | None -> ""
                | Some goals -> 
                    let sb = System.Text.StringBuilder ()
                    ignore <| sb.Append (":-\n")
                    ignore <| sb.Append (' ', 4)
                    ignore <| goals.BuildString 4 sb
                    string sb
            )

type [<Measure>] args
type PredDefn = PredDefn of PredicateName * int<args>
type RuleMap = Map<PredDefn, Predicate seq>

type RuleBase = {
    Rules : RuleMap
}

let rec solve (rb : RuleBase) (predName : PredicateName) (constraints : Constraints) (args : Constraint[]) = 
    match rb.Rules.TryFind (PredDefn (predName, args.Length * 1<args>) ) with
    | None -> Seq.empty
    | Some preds ->
        seq {
            for pred in preds do
                yield! solveOnePred pred rb constraints args
        }

and solveOnePred (pred : Predicate) (rb : RuleBase) (constraints : Constraints) (args : Constraint[]) = 
    
    assert(pred.Head.Args.Length = args.Length)
    match resolve pred.Head.Args constraints args with
    | Some (state) ->
        let rec resolveGoals (state : State) (clause : Clause) = 
//            let g = Guid.NewGuid().ToString("N").[0..5]
            match clause with
            | Goal goalPred ->
                match goalsToConstraints goalPred.Args state with
                | None -> Seq.empty
                | Some (state, args) ->
                    let solutions = solve rb goalPred.Name state.Constraints args
//                    printfn "\n---------"
//                    printfn "%s Goal    : %s"   g <| string goalPred
//                    printfn "%s State   : %s"  g <| string state
//                    printfn "%s Solutions:" g 
//                    solutions |> Seq.iter (string >> printfn "%s %s" g)
                    solutions |> Seq.map (fun soln -> { state with Constraints = soln })
            
            | Conj (leftClause, rightClause) ->
                seq {
                    let leftSolutions = resolveGoals state leftClause
//                    printfn "\n---------"
//                    printfn "> %s Left  : %s"   g <| string leftClause
//                    printfn "> %s State : %s"  g <| string state
//                    printfn "> %s Solutions:" g 
                    for state in leftSolutions do
//                        state    |> (string >> printfn "> %s %s" g)
                        yield! resolveGoals state rightClause
                }
            
            | Disj (leftClause, rightClause) ->
                seq {
                    yield! resolveGoals state leftClause
                    yield! resolveGoals state rightClause
                }

        match pred.Goals with
        | Some goals ->
            resolveGoals state goals
            |> Seq.map (fun state -> state.Constraints)
        | None ->
            Seq.singleton state.Constraints
    | None ->
        Seq.empty

let solveNew (rb : RuleBase) (predName : PredicateName) (args : GoalArg[]) = 
    match goalsToConstraints args State.Empty with
    | None -> Seq.empty
    | Some (state, args) ->
        seq {
            let solutions = solve rb predName state.Constraints args
            for solution in solutions ->
                { state with
                    Constraints = solution
                }
        }
