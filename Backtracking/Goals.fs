module BackTrack.Goals

open Unification

type Local = 
    | Local of string
with
    member x.L =
        match x with
        | Local s -> s
    override x.ToString () =
        "$" + x.L

type GoalArg = 
    | Atom      of Atom
    | Compound  of GoalCompound
    | LocalVar  of Local

    override x.ToString () =
        match x with
        | Atom     c -> string c
        | Compound c -> string c
        | LocalVar l -> string l

and GoalCompound = {
    Tag  : int<compound>
    Args : GoalArg[] 
}
with
    override x.ToString () = 
        let argStrs = Array.map string x.Args
        let printDefault () = 
            sprintf "Compound<%d>(%s)" <| x.Tag / 1<_> <| String.concat ", " argStrs

        match CompoundFormatters.TryFormat (x.Tag, argStrs) with
        | Some s -> s
        | None ->
            printDefault ()
    
    static member New tag args = { Tag = tag; Args = args }

//
type LocalMap = Map<Local, Constraint>
type State = {
    Locals      : LocalMap
    Constraints : Constraints
}
with
    override x.ToString () = 
        sprintf "{%s}[%s]" <| string x.Constraints <| string x.Locals

    static member Create (constraints : Constraints) = 
        {
            Locals = Map.empty
            Constraints = constraints
        }
    static member Empty = 
        {
            Locals = Map.empty
            Constraints = Constraints.Create Map.empty
        }
    member state.ConstraintFor (l : Local) = 
        match state.Locals.TryFind l with
        | Some c ->
            state, c
        | None ->
            let constraints, pointer = state.Constraints.Alloc ()
            let constr = Equals pointer
            { state with
                Constraints = constraints
                Locals      = state.Locals.Add (l, constr)
            }, constr

let rec goalsToConstraints (goals : GoalArg[]) (state : State) = 
    let rec translateNext (state : State) = function
        | GoalArg.Atom a ->
            Some (state, Constraint.Comp (CompConstraint.Atom a))
        | GoalArg.LocalVar l ->
            Some (state.ConstraintFor l)
        | GoalArg.Compound c ->
            match goalsToConstraints c.Args state with
            | Some (state, constrs) ->
                Some (state, Constraint.Comp (CompConstraint.Compound { Tag = c.Tag; Args = constrs }))

            | None -> None

    let rec loop (state : State) (acc : Constraint[]) (i) = 
        if i = goals.Length
        then
            Some (state, acc)
        else
            match translateNext state goals.[i] with
            | Some (state, constr) ->
                acc.[i] <- constr
                loop state acc (i + 1)
            | None ->
                None
    loop state (Array.zeroCreate goals.Length) 0

let resolve (goals : GoalArg[]) (constraints0 : Constraints) (args : Constraint[]) =
    assert(goals.Length = args.Length)

    match goalsToConstraints goals (State.Create constraints0) with
    | Some (state, goalConstrs) ->

        let rec loop (constraints : Constraints) (i) = 
            if i = goalConstrs.Length
            then
                Some constraints
            else
                match unify goalConstrs.[i] args.[i] constraints with
                | Some constraints ->
                    loop (constraints) (i + 1)
                | None ->
                    None
        
        loop state.Constraints 0
        |> Option.map (fun constraints ->
            { state with
                Constraints = constraints
            }
        )
    
    | None ->
        None
