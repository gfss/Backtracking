[<AutoOpen>]
module Common.FSharp.CollectionExt

type private StepThrough = System.Diagnostics.DebuggerNonUserCodeAttribute

let inline (|GivesSome|_|) f x = 
    match f x with
    | Some x -> Some x
    | None   -> None

let inline uncurry2 f h (a, b) = f h a b

let (|Argto|) f x = f x

module Loops =
    let inline takeLast f s = 
        let mutable s = s
        while (
                let fs = f s
                match fs with
                | Some fs ->
                    s <- fs
                    true
                | None ->
                    false
        ) do ()
        s
        
    let inline advanceUntil (f : 's -> bool) (adv : 's -> 's) (s : 's) = 
        let mutable s = s
        while not (f s) do
            s <- adv s
        s
        
    let inline initFold (i: int) (f : int -> 's -> 's) (s : 's) = 
        let mutable s = s
        for i in 0 .. i - 1 do
            s <- f i s
        s

    let inline advanceWhile (f) = advanceUntil (not << f)

module Array = 
    let inline foldPair     (f : 'State -> 'T -> 'State)    = uncurry (Array.fold f)
    let inline foldBackPair (f : 'T -> 'State -> 'State)    = uncurry (Array.foldBack f)
    let inline fold23       (f : 'State -> 'T1 -> 'T2 -> 'State) (s, t, u) = Array.fold2 f s t u
    let inline scanPair     (f : 'State -> 'T -> 'State)    = uncurry (Array.scan f)
    let inline scanBackPair (f : 'T -> 'State -> 'State)    = uncurry (Array.scanBack f)
    let inline map2Pair     (f : 'T1 -> 'T2 -> 'U)          = uncurry (Array.map2 f)
    let inline mapi2Pair    (f : int -> 'T1 -> 'T2 -> 'State) (t, u) = Array.mapi2 f t u
    let inline iter2Pair    (f : 'T1 -> 'T2 -> unit)        = uncurry (Array.iter2 f)
    let inline fold2Tuple   (f : 'State -> 'T1 -> 'T2 -> 'State) (s, t, u) = Array.fold2 f s t u

    let inline foldWhile (f : 'State -> 'T -> 'State option) (state : 'State) (ts : 'T[]) : 'State option =
        let rec doit i = 
            if i = ts.Length
            then
                Some state
            else
                match f state ts.[i] with
                | Some nextState ->
                    doit (i + 1)
                | None ->
                    None
        doit 0
        
    let inline foldi (f : int -> 'State -> 'T -> 'State) (state : 'State) (ts : 'T[]) : 'State =
        let mutable state = state
        for i in 0 .. ts.Length - 1 do
            state <- f i (state) (ts.[i])
        state
        
    let inline foldiPair (f : int -> 'State -> 'T -> 'State) (state : 'State, ts : 'T[]) : 'State =
        foldi f state ts
        
    [<StepThrough>]
    let scrambleInPlace (arr : 'T[], random : System.Random) = 
        for i in 0 .. arr.Length - 2 do
            let swapWith = random.Next (i, arr.Length)
            if i <> swapWith
            then
                let tmp = arr.[i]
                arr.[i]        <- arr.[swapWith]
                arr.[swapWith] <- tmp
        ()

    let inline back (ts : 'T[]) = ts.[ts.Length - 1]

    //
    let inline initFold (i: int) (f : int -> 'State -> ('State * 'T)) (state : 'State) : 'State * 'T[] = 
        let mutable state = state
        let ts = Array.zeroCreate i
        for i in 0 .. ts.Length - 1 do
            let s, t = f i (state)
            ts.[i] <- t
            state <- s
        (state, ts)

    //
    let inline mapFold (f : 'State -> 'T -> ('State * 'U)) (state : 'State) (ts : 'T[]) : 'State * 'U[] = 
        let mutable state = state
        let us = Array.zeroCreate ts.Length
        for i in 0 .. ts.Length - 1 do
            let s, u = f (state) (ts.[i])
            us.[i] <- u
            state <- s
        (state, us)
    
    let inline mapFoldPair     f (state : 'State, ts : 'T[])      = mapFold f state ts

    //
    let inline mapFoldi (f : int -> 'State -> 'T -> ('State * 'U)) (state : 'State) (ts : 'T[]) : 'State * 'U[] = 
        let mutable state = state
        let us = Array.zeroCreate ts.Length
        for i in 0 .. ts.Length - 1 do
            let s, u = f i (state) (ts.[i])
            us.[i] <- u
            state <- s
        (state, us)
    
    let inline mapFoldiPair     f (state : 'State, ts : 'T[])      = mapFoldi f state ts

    //
    let inline map2Fold (f : 'State -> ('T1 * 'T2) -> ('State * 'U)) (state : 'State) (t1s : 'T1[]) (t2s : 'T2[]) : 'State * 'U[] = 
        let mutable state = state
        let us = Array.zeroCreate t1s.Length
        for i in 0 .. t1s.Length - 1 do
            let s, u = f (state) (t1s.[i], t2s.[i])
            us.[i] <- u
            state <- s
        (state, us)
    
    let inline map2FoldPair     f (state : 'State, ts : 'T[], us : 'T[])      = mapFold f state ts
        
    //
    [<StepThrough>]
    let replace (i : int) (v : 't) (arr : 't[]) = 
        let result = Array.zeroCreate (arr.Length)
        for i in 0 .. i - 1 do
            result.[i] <- arr.[i]
        for i in i + 1 .. arr.Length - 1 do
            result.[i] <- arr.[i]
        result.[i] <- v
        result

    //
    [<StepThrough>]
    let insert (i : int) (v : 't) (arr : 't[]) = 
        let result = Array.zeroCreate (arr.Length + 1)
        for i in 0 .. i - 1 do
            result.[i] <- arr.[i]
        result.[i] <- v
        for i in i .. arr.Length - 1 do
            result.[i + 1] <- arr.[i]
        result

    //
    let inline pushBack (v : 't) (arr : 't[]) = insert arr.Length v arr
        

    //
    [<StepThrough>]
    let remove (i : int) (arr : 't[]) = 
        let result = Array.zeroCreate (arr.Length - 1)
        for i in 0 .. i - 1 do
            result.[i] <- arr.[i]
        for i in i .. result.Length - 1 do
            result.[i] <- arr.[i + 1]
        result

module Seq =                                          
    let inline foldPair     f (state : 'State, ts : 'T seq)    = Seq.fold f state ts
    let inline scanPair     f (state : 'State, ts : 'T seq)    = Seq.scan f state ts
    let inline map2Pair     f (t1 : 'T1 seq, t2 : 'T2 seq)     = Seq.map2 f t1 t2
    let inline iter2Pair    f (t1 : 'T1 seq, t2 : 'T2 seq)     = Seq.iter2 f t1 t2
    
    let inline createFromGetEnumerator<'ge when ^ge : (member GetEnumerator : unit -> System.Collections.IEnumerator)> (ie : 'ge) = 
        let e = (^ge : (member GetEnumerator : unit -> System.Collections.IEnumerator) ie)
        seq {
            while e.MoveNext () do
                yield e.Current
        }
        
    [<StepThrough>]
    let rec foldWhile (f : 'State -> 'T -> 'State option) (state : 'State) (ts : 'T seq) : 'State option =
        use e = ts.GetEnumerator ()
        let rec doit () = 
            match e.MoveNext () with
            | false ->
                Some state
            | true ->
                match f state e.Current with
                | Some nextState ->
                    doit ()
                | None ->
                    None
        doit ()
        
    let inline foldUntil (advance : 'S -> 'T -> 'S * bool) (state: 'S) (ts : 'T seq) : 'S * bool = 
        use enumerator = ts.GetEnumerator ()
        let mutable state = state
        let mutable stop = false
        while (not stop && enumerator.MoveNext ()) do
            let nextState, stopNow  = advance state enumerator.Current
            state <- nextState
            stop <- stopNow
        state, stop

    [<System.Diagnostics.DebuggerNonUserCodeAttribute>]
    let tryPeek (s: seq<_>) = Seq.tryFind (fun _ -> true) s
    
    [<System.Diagnostics.DebuggerNonUserCodeAttribute>]
    let exclude v = Seq.filter ((<>) v)


module List = 
    let inline (|ExpectHead|) list =
        let h, t = List.head list, List.tail list
        h, t

    let inline foldPair     f (state : 'State, ts : 'T list)    = List.fold f state ts
    let inline foldBackPair f (ts : 'T list, state : 'State)    = List.foldBack f ts state
    let inline map2Pair     f (t1 : 'T1 list, t2 : 'T2 list)     = List.map2 f t1 t2
    let inline iter2Pair    f (t1 : 'T1 list, t2 : 'T2 list)     = List.iter2 f t1 t2
    let inline fold2Tuple   (f : 'State -> 'T1 -> 'T2 -> 'State) (s, t, u) = List.fold2 f s t u
    
    [<StepThrough>]
    let revAppend = 
        let rec loop acc tail =
            match acc with
            | ah :: at -> loop (at) (ah :: tail)
            | [] -> tail
        loop
            
    [<StepThrough>] 
    let tryRemoveIf (test : 'T -> bool) (ts : 'T list) =
        let rec loop (acc) (tail) = 
            match tail with
            | [] -> None
            | h :: tail ->
                if test h then
                    Some (h, revAppend acc tail)
                else
                    loop (h :: acc) tail
        loop [] ts
             
    let inline tryRemove (find : 'T) (ts : 'T list) =
        tryRemoveIf ((=) find) ts
        
    [<StepThrough>]
    let rec foldWhile (f : 'State -> 'T -> 'State option) (state : 'State) (ts : 'T list) : 'State option =
        let rec doit ts = 
            match ts with
            | [] ->
                Some state
            | h :: t ->
                match f state h with
                | Some nextState ->
                    doit t
                | None ->
                    None
        doit ts
        
    [<StepThrough>]
    let revToArray (ts : 'T list) =
        let count = List.length ts
        let arr = Array.zeroCreate count
        ts |> List.iteri (fun i t -> arr.[count - i - 1] <- t)
        arr

    //
    [<StepThrough>]
    let mapFold (f : 'State -> 'T -> ('State * 'U)) (state : 'State) (ts : 'T list) : 'State * 'U list = 
        let mutable state = state
        let mutable acc  = []
        let mutable rest = ts
        while (
                match rest with
                | h :: tail ->
                    let s, u = f state h
                    state <- s
                    acc <- u :: acc
                    rest <- tail
                    true
                | [] -> false
        ) do ()
        (state, List.rev acc)
    
    let inline mapFoldPair     f (state : 'State, ts : 'T list)      = mapFold f state ts


module Set = 
    let inline foldPair     (f : 'State -> 'T -> 'State)    = uncurry (Set.fold f)
    let inline maybeAdd   v s = match v with Some v -> Set.add v s | None -> s
    let inline maybeUnion v s = match v with Some v -> Set.union v s | None -> s

module Map = 
    [<StepThrough>]
    let removeMany (keys: #seq<'k>) (map: Map<'k,'v>) = 
        let mutable map = map
        for key in keys do
            map <- map.Remove key
        map
        
    [<StepThrough>]
    let addToSet key item map = 
        Map.add key
            (
                match Map.tryFind key map with
                | Some set -> set
                | None -> Set.empty
                |> Set.add item
            ) map

    [<StepThrough>]
    let unionToSet key item map = 
        Map.add key
            (
                match Map.tryFind key map with
                | Some set -> set |> Set.union item
                | None -> item
            ) map

type SetMap<'k,'v when 'k : comparison and 'v: comparison> = Map<'k, Set<'v>>

module SetMap = 
    let remove k v (setmap: SetMap<'k,'v>) = 
        match setmap.TryFind k with
        | Some vs ->
            let vs = (vs.Remove v)
            if vs.IsEmpty then
                Map.remove k setmap
            else
                Map.add k vs setmap
        | None -> setmap

    let removeKey = Map.remove

    let add k v (setmap: SetMap<'k,'v>) = 
        setmap
        |> Map.add k (
            match setmap.TryFind k with
            | Some vs -> vs.Add v
            | None -> Set.singleton v
        )

module Option =
    let ofTestResult = function | false, _ -> None | true, v -> Some v

    let ofChoice = function | Choice1Of2 v -> Some v | _ -> None
    let inline defaultsTo v = function | Some v -> v | None -> v
    let inline defaultsTof f = function | Some v -> v | None -> f ()
    let inline bindNone f = function | Some v -> Some v | None -> f ()
    let inline iterNone f = function | Some _ -> () | None -> f ()
    let inline filter f = function | Some v when f v -> Some v | _ -> None
    let wrapByrefFunc (f: 'v -> bool * 'r) = fun (v: 'v) ->
        match f v with
        | true, r -> Some r
        | false, r -> None
    let inline ofNullable<'o when 'o: null> (o: 'o) =
        match o with
        | null -> None
        | o -> Some o
        
    let inline ifDifferent a b = 
        if a = b then
            None
        else
            Some b
    
type Option<'t> with
    member inline x.Map f = Option.map f x
    member inline x.DefaultTo v = Option.defaultsTo v x

module Choice = 
    let Isc1 = function
        | Choice1Of2 (va : 'a) -> true
        | Choice2Of2 (vb : 'b) -> false

    let get1 = function
        | Choice1Of2 (va : 'a) -> va
        | Choice2Of2 (vb : 'b) -> failwithf "Expected Choice1Of2"

    let get2 = function
        | Choice1Of2 (va : 'a) -> failwithf "Expected Choice2Of2"
        | Choice2Of2 (vb : 'b) -> vb

    let getOrRaise = function
        | Choice1Of2 (va : 'a) -> va
        | Choice2Of2 (vb : exn) -> raise vb

    let test1 = function
        | Choice1Of2 (va : 'a) -> Some va
        | Choice2Of2 (b : 'b) -> None

    let test2 = function
        | Choice1Of2 (va : 'a) -> None
        | Choice2Of2 (vb : 'b) -> Some vb
        
    let Isc2 = function
        | Choice1Of2 (va : 'a) -> false
        | Choice2Of2 (vb : 'b) -> true

    let inline Mapc1 (f : 'a -> 'a1) = function
        | Choice1Of2 (va : 'a) -> Choice1Of2 (f va)
        | Choice2Of2 (vb : 'b) -> Choice2Of2 vb

    let inline Mapc2 (f : 'b -> 'b1) = function
        | Choice1Of2 (va : 'a) -> Choice1Of2 va     
        | Choice2Of2 (vb : 'b) -> Choice2Of2 (f vb)
        
    let inline Bindc1 (f : 'a -> Choice<'a2,'b>) = function
        | Choice1Of2 (va : 'a) -> f va
        | Choice2Of2 (vb : 'b) -> Choice2Of2 vb

type Tuple = 
    static member inline uncurry1 fu (a                     ) = fu a
    static member inline uncurry2 fu (a, b                  ) = fu a b
    static member inline uncurry3 fu (a, b, c               ) = fu a b c
    static member inline uncurry4 fu (a, b, c, d            ) = fu a b c d
    static member inline uncurry5 fu (a, b, c, d, e         ) = fu a b c d e
    static member inline uncurry6 fu (a, b, c, d, e, f      ) = fu a b c d e f
    static member inline uncurry7 fu (a, b, c, d, e, f, g   ) = fu a b c d e f g
    static member inline uncurry8 fu (a, b, c, d, e, f, g, h) = fu a b c d e f g h
    
    static member inline arity (a                     ) = 1
    static member inline arity (a, b                  ) = 2
    static member inline arity (a, b, c               ) = 3
    static member inline arity (a, b, c, d            ) = 4
    static member inline arity (a, b, c, d, e         ) = 5
    static member inline arity (a, b, c, d, e, f      ) = 6
    static member inline arity (a, b, c, d, e, f, g   ) = 7
    static member inline arity (a, b, c, d, e, f, g, h) = 8


module Pair = 
    let inline toArray (a, b) = [| a; b |]
    let inline create a b = (a, b)
    let inline apply (fa, fb) (a, b) = (fa a, fb b)
    let inline map   (f) (a, b) = (f a, f b)
    let inline sort  (a, b) = if a <= b then (a, b) else (b, a)
    let inline keyFor (f) a = a, (f a)


type System.String
with
    member inline text.GetLines () = text.Split ([|"\r\n"; "\n"|], System.StringSplitOptions.None)

module String = 
    let inline FromSeq sep (ts: 't seq) = 
        ts |> Seq.map string |> String.concat sep
        
    [<StepThrough>]
    let remove (index: int) (count: int) (source: string) =
        if count < 0 then invalidArg "count" "Can't remove negative count"
        if index < 0 then invalidArg "index" "index < 0"
        if index >= source.Length then invalidArg "index" "index >= source.Length"
        if count + index > source.Length then invalidArg "count" "count + index >= source.Length"

        if count = 0 then
            source
        else
            let sb = System.Text.StringBuilder (source.Length - count)
            sb.Append (source, 0, index)                      |> ignore
            sb.Append (source, index + count, source.Length - (index + count))  |> ignore
            sb.ToString ()

    [<StepThrough>]
    let insert (index: int) (insertion: string) (source: string) =
        if index < 0 then invalidArg "index" "index < 0"
        if index > source.Length then invalidArg "index" "index >= source.Length"

        if insertion.Length = 0 then
            source
        elif source.Length = 0 then
            insertion
        else
            let sb = System.Text.StringBuilder (source.Length + insertion.Length)
            sb.Append (source, 0, index)                      |> ignore
            sb.Append insertion                               |> ignore
            sb.Append (source, index, source.Length - index)  |> ignore
            sb.ToString ()

    let inline toLower (s: string) = s.ToLower ()

type System.Collections.Generic.IDictionary<'k,'v>
with
    member inline x.TryFind (key) =
        match x.TryGetValue key with
        | true, v -> Some v
        | false, _ -> None

type System.Collections.Generic.List<'t>
with
    member inline x.Back       = x.[x.Count-1]
    member inline x.PopBack () = let r = x.Back in x.RemoveAt (x.Count-1); r

module KeyValue = 
    let tuple (kvp: System.Collections.Generic.KeyValuePair<'k,'v>) = kvp.Key, kvp.Value
    let key (kvp: System.Collections.Generic.KeyValuePair<'k,'v>) = kvp.Key
    let value (kvp: System.Collections.Generic.KeyValuePair<'k,'v>) = kvp.Value

type System.Console
with
    static member inline SkipKey () = System.Console.ReadKey () |> safe.ignore

type System.DateTime
with
    member x.MillisecondTimeString = x.ToString("HH:mm:ss.fff")
    member x.ToMillisecondTimeString () = x.ToString("HH:mm:ss.fff")

type System.Random
with
    member x.NextDouble (lower, upper) = 
        let lower, upper = Pair.sort (lower, upper)
        x.NextDouble () * (upper - lower) + lower

type System.Random with
    member x.NextSeed () = x.Next () * 1<seed>

type System.Random with
    static member seeded (seed: int<seed>) = System.Random (int seed)

module IDisposable =
    let Null = { new System.IDisposable with member x.Dispose () = () }
    let Create = fun f -> 
        { new System.IDisposable with
            member x.Dispose () = f()
        }
    let CreateOnce = fun f ->
        let disposed = ref 0
        { new System.IDisposable with
            member x.Dispose () =
                if 0 = System.Threading.Interlocked.Exchange (disposed, 1)
                then
                    f ()
        }

let tau = System.Math.PI * 2.
type System.Math
with
    static member inline Tau = tau
    static member inline Square (x) = x * x
    static member inline ClampTo (low, hi) x =
        if x < low then low
        elif hi < x then hi
        else x
    /// linear interpolation
    static member inline Lerp (low, hi, ratio) = low * (1. - ratio) + hi * ratio