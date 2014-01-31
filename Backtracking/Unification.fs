module BackTrack.Unification

///
type Atom =
    | AString of string
    | ANum    of int
with
    override x.ToString () =
        match x with
        | AString s -> s
        | ANum s -> string s

///
type Pointer =
    | Pointer of string
with
    member x.P =
        match x with
        | Pointer s -> s
    override x.ToString () =
        "&" + x.P

///
type [<Measure>] compound

[<AutoOpen>]
module private Internal =
    type [<Measure>] args
    type FixLenFormatTag = int<compound> * int<args>
    type VarLenFormatTag = int<compound>
    
type Compound = {
    Tag  : int<compound>
    Args : Constraint[] 
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

///
and CompConstraint =
    | Atom   of Atom
    | Compound of Compound
with
    override x.ToString () =
        match x with
        | Atom a ->
            sprintf "#%s" <| string a
        | Compound s ->
            sprintf "%s" <| string s

and Constraint = 
    | Comp   of CompConstraint
    | Equals of Pointer
with
    override x.ToString () =
        match x with
        | Comp s ->
            string s
        | Equals p ->
            string p
                
and CompoundFormatters () = 
    static let mutable varLenFormatters : Map<VarLenFormatTag, string[] -> string> = Map.empty
    static let mutable fixLenFormatters : Map<FixLenFormatTag, string[] -> string> = Map.empty
    static let mutable newTag = 0x10000<compound>

    static member TryFormat (tag, args : string[]) =    
        match fixLenFormatters.TryFind ((tag, args.Length * 1<args>)) with
        | Some formatter ->
            Some (formatter args)
        | None ->
            match varLenFormatters.TryFind tag with
            | Some formatter ->
                Some (formatter args)
            | None ->
                None

    static member TryFormat (compound : Compound) =
        CompoundFormatters.TryFormat (compound.Tag, compound.Args |> Array.map string)

    ///
    static member private RegisterAt (tag, args, f) =
        let key = (tag, args)
        if fixLenFormatters.ContainsKey key
        then
            assert(false)
            failwithf "tag %A already registered" key
        else
            fixLenFormatters <- fixLenFormatters.Add (key, f)

    static member Register (f : string[] -> string)  = 
        let tag = CompoundFormatters.NewTag ()
        varLenFormatters <- Map.add tag f varLenFormatters
        tag

    ///
    static member internal NewTag () =
        newTag <- newTag + 1<_>
        newTag

type CompoundFormatters
with 
    ///
    static member Overload (tag, s) = 
        CompoundFormatters.RegisterAt (tag, 0<args>, fun (arr : _[]) ->
            assert(arr.Length=0)
            s
        )
    static member Overload (tag, f : string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 1<args>, fun (arr : _[]) ->
            assert(arr.Length=1)
            f (arr.[0])
        )
    static member Overload (tag, f : string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 2<args>, fun (arr : _[]) ->
            assert(arr.Length=2)
            f (arr.[0], arr.[1])
        )
    static member Overload (tag, f : string * string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 3<args>, fun (arr : _[]) ->
            assert(arr.Length=3)
            f (arr.[0], arr.[1], arr.[2])
        )
    static member Overload (tag, f : string * string * string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 4<args>, fun (arr : _[]) ->
            assert(arr.Length=4)
            f (arr.[0], arr.[1], arr.[2], arr.[3])
        )
    static member Overload (tag, f : string * string * string * string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 5<args>, fun (arr : _[]) ->
            assert(arr.Length=5)
            f (arr.[0], arr.[1], arr.[2], arr.[3], arr.[4])
        )
    static member Overload (tag, f : string * string * string * string * string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 6<args>, fun (arr : _[]) ->
            assert(arr.Length=6)
            f (arr.[0], arr.[1], arr.[2], arr.[3], arr.[4], arr.[5])
        )
    static member Overload (tag, f : string * string * string * string * string * string * string -> string)  = 
        CompoundFormatters.RegisterAt (tag, 7<args>, fun (arr : _[]) ->
            assert(arr.Length=7)
            f (arr.[0], arr.[1], arr.[2], arr.[3], arr.[4], arr.[5], arr.[6])
        )
    
    ///
    static member Register (s : string) = 
        let tag = CompoundFormatters.NewTag ()
        CompoundFormatters.Overload (tag, s)
        tag

    static member Register (f : string -> string) = 
        let tag = CompoundFormatters.NewTag ()
        CompoundFormatters.Overload (tag, f)
        tag

    static member Register (f : string * string -> string)  = 
        let tag = CompoundFormatters.NewTag ()
        CompoundFormatters.Overload (tag, f)
        tag
        
    static member Register (f : string * string * string -> string)  = 
        let tag = CompoundFormatters.NewTag ()
        CompoundFormatters.Overload (tag, f)
        tag

    static member Register (f : string * string * string * string -> string)  = 
        let tag = CompoundFormatters.NewTag ()
        CompoundFormatters.Overload (CompoundFormatters.NewTag (), f)
        tag

//
type ConstraintMap = Map<Pointer, Constraint option>
type Constraints = {
    Map : ConstraintMap
    N   : int
}
with
    override x.ToString () = 
        x.Map
        |> Seq.map (fun (KeyValue(k, v)) ->
            k.P +
                match v with
                | None -> "_"
                | Some v -> string v
        )
        |> String.concat "; "
        |> sprintf "{ %s }"

    static member Create map =
        let r = System.Random ()
        {
            Map = map
            N   = 0
        }

    member constraints.derefConstraint (c : Constraint) = 
        match c with
        | Constraint.Comp c -> Choice1Of2 c
        | Constraint.Equals p -> 
            match constraints.Map.[p] with
            | None -> Choice2Of2 p
            | Some c ->
                constraints.derefConstraint c

    member constraints.simplifyConstraint (c : Constraint) = 
        match c with
        | Constraint.Comp c ->
            Constraint.Comp <|
                match c with
                | CompConstraint.Atom a -> CompConstraint.Atom a
                | CompConstraint.Compound comp ->
                    CompConstraint.Compound 
                        <|
                        { comp with
                            Args = comp.Args |> Array.map constraints.simplifyConstraint
                        }
        | Constraint.Equals p -> 
            match constraints.Map.[p] with
            | None -> Constraint.Equals p
            | Some c ->
                constraints.simplifyConstraint c

    member constraints.deref (p : Pointer) = 
        match constraints.Map.[p] with
        | None -> p, None
        | Some r ->
            match r with
            | Comp (s) -> p, Some s
            | Equals p ->
                constraints.deref p

    member constraints.Alloc () = 
        let pointer = Pointer (sprintf "_G%d" (constraints.N + 1))
        { constraints with
            Map = constraints.Map.Add (pointer, None)
            N   = constraints.N + 1
        }, pointer

// -------------------------------------
// ---- Unification --------------------
let inline deref (constraints : Constraints) (p : Pointer) = constraints.deref p

let rec unifyCompConstraints (constraints : Constraints) (v1 : CompConstraint) (v2 : CompConstraint) = 
    match v1, v2 with
    | Atom a1, Atom a2 ->
        if a1 = a2
        then Some constraints
        else None
    | Atom _, _ ->
        None
    | Compound s1, Compound s2 ->
        if s1.Tag = s2.Tag && s1.Args.Length = s2.Args.Length
        then
            let rec doit (constraints : Constraints) i = 
                if i = s1.Args.Length
                then
                    Some constraints
                else
                    match unify s1.Args.[i] s2.Args.[i] constraints with
                    | Some nextConstraints ->
                        doit nextConstraints (i + 1)
                    | None ->
                        None
            doit constraints 0
        else
            None
    | Compound _, _ ->
        None

and unifyPointers (constraints : Constraints) (p1 : Pointer) (p2 : Pointer) = 
    let pstore1, store1 = deref constraints p1
    let pstore2, store2 = deref constraints p2
    if pstore1 = pstore2
    then
        Some constraints
    else
        match store1, store2 with
        | Some v1, Some v2 ->
            unifyCompConstraints constraints v1 v2
        | None, _ ->
            Some
                { constraints with
                    Map = constraints.Map.Add (pstore1, Some (Equals pstore2))
                }
        | Some _, _ ->
            Some
                { constraints with
                    Map = constraints.Map.Add (pstore2, Some (Equals pstore1))
                }
    
and unifyPointerCompConstraint (constraints : Constraints) (p : Pointer) (v : CompConstraint) = 
    let pstore, store = deref constraints p
    match store with
    | Some v1 ->
        unifyCompConstraints constraints v1 v
    | None ->
        Some
            { constraints with
                Map = constraints.Map.Add(pstore, Some (Comp v ))
            }

and unifyPointerWith (constraints : Constraints) (p : Pointer) = function
    | Comp v ->
        unifyPointerCompConstraint constraints p v
    | Equals p2 ->
        unifyPointers constraints p p2

and unify (v1 : Constraint) (v2 : Constraint) (constraints : Constraints) = 
    match v1, v2 with
    | Comp a1, Comp a2 ->
        unifyCompConstraints constraints a1 a2

    | Comp a, Equals p
    | Equals p, Comp a ->
        unifyPointerCompConstraint constraints p a

    | Equals p1, Equals p2 ->
        unifyPointers constraints p1 p2

// -------------------------------------
// ---- Tests --------------------------