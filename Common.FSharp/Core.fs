[<AutoOpen>]
module Common.FSharp.Core

let inline hconst x _ = x

let inline curry f x y = f (x, y)

let inline uncurry f (x, y) = f x y
let inline uncurry2 f w (x, y) = f w x y

let inline flip f x y = f y x

let inline delay f a = fun () -> f a
let inline delayConst f a = fun _ -> f a

let inline dispose (d : System.IDisposable) = d.Dispose()

let inline tee f x = f x; x
let inline (|>!) x f = f x; x
let inline (|>.) x f = x, (f x)

let inline (&&&=) a b = a &&& b = b

let inline (===&) a b = System.Object.ReferenceEquals (a, b)

let inline (|Passes|_|) f a = if f a then Some () else None

let inline (|Equals|_|) a b = if b = a then Some () else None

let inline (|GivesSome|_|) f a = f a

let inline (|Gives|) f a = (f a)

let inline bind0 f x = f x
let inline bind1 f x a = f a x
let inline bind2 f x a b = f a b x
let inline bind3 f x a b c = f a b c x
let inline bind4 f x a b c d = f a b c d x
let inline bind5 f x a b c d e = f a b c d e x

type safe =
    static member inline ignore (t : 't -> 'u) = () // this will be ambiguous
    static member inline ignore (t : 't) = ()

let inline impossible () = assert(false); failwith "that should be impossible"
    
let mutable TODOAssertEnabled = true

[<System.ObsoleteAttribute "TODO">]
let inline TODO _ =
    assert(not TODOAssertEnabled);
    failwith "TODO"
    
[<System.ObsoleteAttribute "TODO0">]
let inline TODO0 v = v

let TryCast<'t> (o : obj) = match o with | :? 't as t -> Some t | _ -> None

let runOnce f = 
    let r = ref 0
    fun () ->
        if System.Threading.Interlocked.Increment r = 1 then
            f ()
            
/// Pass ref state to function that returns state-value tuple;
/// store new state in ref; return value
let inline CallStoreRet (f: 'state -> 'state * 'ret) store = let s, r = f !store in store := s; r
let inline CallStore (f: 'state -> 'state) store = let s = f !store in store := s

let inline CallStoreRetAdv (f: 'state -> 'state * 'ret) store = let s, r = f (snd !store) in store := (Some (snd !store), s); r
let inline CallStoreAdv (f: 'state -> 'state) store = let s = f (snd !store) in store := (Some (snd !store), s)

type [<Measure>] seed
