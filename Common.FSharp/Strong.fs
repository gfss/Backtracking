namespace Common.FSharp

type StrongType<'v, [<Measure>] 'u> = 
    abstract Weaken : 'v

#nowarn "1240" // unit-of-measure warning
[<CustomEqualityAttribute>]
[<NoComparisonAttribute>]
type Strong<'v,[<Measure>] 'u> = 
    struct
        val Value : 'v
        
        new (value : 'v) = { Value = value }
    end

    interface StrongType<'v,'u> with
        member x.Weaken = x.Value

    member x.Weaken = x.Value

    static member convert<[<Measure>] 'w> (x: Strong<'v,'u>) = Strong<_, 'u * 'w> (x.Value)
    
    override x.ToString () = x.Value.ToString ()
    
    override x.GetHashCode () = (box x.Value).GetHashCode ()
    
    override x.Equals (y : obj) = 
        match y with
        | null                  -> box x.Value = null
        | :? 'v    as y         -> box x.Value = box y
        | :? Strong<'v,1> as y  -> box x.Value = box y.Value
        | _ -> false

    
and [<CustomEqualityAttribute>]
    [<CustomComparisonAttribute>]
    StrongCmp<'v,[<Measure>] 'u when 'v : comparison> = 

    struct
        val Value : 'v
        
        new (value : 'v) = { Value = value }
        new (strong : Strong<'v,'u>) = { Value = strong.Value }
        new (strong : StrongCmp<'v,'u>) = { Value = strong.Value }
    end

    interface StrongType<'v,'u> with
        member x.Weaken = x.Value

    member x.Weaken = x.Value
    
    member x.Strong = Strong<'v,'u> x.Value

    static member convert<[<Measure>] 'w> (x: StrongCmp<'v,'u>) = StrongCmp<_, 'u * 'w> (x.Value)

    interface System.IComparable<StrongCmp<'v,'u>> with
        member x.CompareTo y = compare x.Value y.Value

    interface System.IComparable with
        member x.CompareTo y = 
            match y with 
            | :? StrongCmp<'v,'u> as y  -> compare x.Value y.Value
            | _                         -> -1
    
    override x.ToString () = x.Value.ToString ()
    
    override x.GetHashCode () = (box x.Value).GetHashCode ()
    
    override x.Equals (y : obj) = 
        match y with
        | null                      -> box x.Value = null
        | :? 'v    as y             -> box x.Value = box y
        | :? StrongCmp<'v,1> as y   -> box x.Value = box y.Value
        | :? Strong<'v,1> as y      -> assert(false); box x.Value = box y.Value
        | _ -> false

type StrongString<[<Measure>] 's> = StrongCmp<string,'s>
type sstring<[<Measure>] 's> = StrongString<'s>
type ss<[<Measure>] 's> = StrongString<'s>

[<AutoOpen>]
module Strong = 

    let inline specialize<'v,[<Measure>] 'u> (v : 'v) = Strong<'v,'u> (v)

    let inline weaken0<'strong, 'v when ^strong: (member Weaken: 'v)> (strong : 'strong) =
        (^strong: (member Weaken: 'v) strong)

    let inline weaken<'v, [<Measure>] 'u, 'strong when ^strong :> StrongType<'v,'u>> (strong : 'strong) =
        strong.Weaken
        
    type System.Int32 with
        static member tryParse<[<Measure>] 'u> (s: ss<'u>) = 
            match Option.ofTestResult (System.Int32.TryParse s.Weaken) with
            | Some i -> Some (LanguagePrimitives.Int32WithMeasure<'u> i)
            | None -> None

    type System.Double with
        static member tryParse<[<Measure>] 'u> (s: ss<'u>) = 
            match Option.ofTestResult (System.Double.TryParse s.Weaken) with
            | Some i -> Some (LanguagePrimitives.FloatWithMeasure<'u> i)
            | None -> None


namespace Common.FSharp.StrongGuid

[<CustomEqualityAttribute>]
[<NoComparisonAttribute>]
type Guid<[<Measure>] 't> = 
    struct
        val Guid : System.Guid
        new (guid : System.Guid) = { Guid = guid }
    end

    static member Empty : Guid<'t> = new Guid<'t> (System.Guid.Empty)

    static member NewGuid () : Guid<'t> = new Guid<'t> (System.Guid.NewGuid ())
    
    override x.ToString () = x.Guid.ToString ()
    member x.ToString (format) = x.Guid.ToString (format)
    member x.ToString (format, provider) = x.Guid.ToString (format, provider)

    override x.GetHashCode () = x.Guid.GetHashCode ()

    member x.ToByteArray () = x.Guid.ToByteArray ()

    override x.Equals (y : obj) = 
        match y with
        | :? Guid<'t>    as y -> x.Guid = y.Guid
        | :? System.Guid as y -> x.Guid = y
        | _ -> false

//    interface Restorable<Guid> with
//        member x.Save = x.Guid
//
//    interface System.IEquatable<Guid<'t>> with
//        member x.Equals y = x.Guid = y.Guid

//    interface Mappers.Restorable<System.Guid> with
//        member x.Save () = x.Guid


//type System.Guid
//with
//    member x.Specialize<'t> () = Guid<'t>(x)
