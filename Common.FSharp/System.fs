module Common.FSharp.System

open System

type Vector<'t> = System.Collections.Generic.List<'t>

let FIN () = 
    printfn "\n[<<<<<<<<<<<<<<<<<< FIN >>>>>>>>>>>>>>>>>>]"
    let _ = System.Console.ReadLine()
    System.Environment.Exit 0

module IO = 

    let stringCallbackStream (encoding: System.Text.Encoding, callback: string -> unit) =
        { new System.IO.Stream () with
            member x.CanRead = false
            member x.CanSeek = false
            member x.CanWrite = true
            member x.Position with get () = raise <| System.InvalidOperationException () and set v = raise <| System.InvalidOperationException ()
            member x.Length = raise <| System.InvalidOperationException ()
            member x.Flush () = ()
            member x.Seek (a, b) = raise <| System.InvalidOperationException ()
            member x.SetLength v = raise <| System.InvalidOperationException ()
            member x.Read (buffer, offset, count) = raise <| System.InvalidOperationException ()
            member x.Write (buffer, offset, count) =
                let chars = encoding.GetChars (buffer, offset, count)
                callback (String (chars: char[]))
        }

    let stringCallbackTextWriter (encoding, callback: string -> unit) = 
        let stream = stringCallbackStream (encoding, callback)
        let writer = new System.IO.StreamWriter(stream, encoding)
        writer.AutoFlush <- true
        writer

    let NewlinePrefixInjectorWriter (prefix) =
        let first = ref true
        let withPrefix = "\n" + prefix
        fun (encoding, out: System.IO.TextWriter) ->
            let stream = stringCallbackStream (encoding, fun s ->
                if !first then
                    out.Write (prefix)
                    first := false
                out.Write (s.Replace ("\n", withPrefix)))
            let writer = new System.IO.StreamWriter(stream, out.Encoding)
            writer.AutoFlush <- true
            writer, IDisposable.Create (fun () -> if not !first then out.WriteLine ())

module Console = 
    type Redirector private (out: System.IO.TextWriter, disposables) =
        let previousOut = Console.Out
        do Console.SetOut ( out )
    

        let mutable disposables =
            IDisposable.Create (fun () ->  
                Console.SetOut previousOut
            )
            :: disposables
        
        new (out) = 
            new Redirector (out, [])
            
        new (out: string -> unit) = 
            let writer = IO.stringCallbackTextWriter (Console.OutputEncoding, out)
            new Redirector (writer, [writer])
            
        new (out: System.Text.StringBuilder) = 
            let writer = IO.stringCallbackTextWriter (Console.OutputEncoding, out.Append >> ignore)
            new Redirector (writer, [writer])
        
        member x.Revert () =
            disposables |> List.iter (dispose)
            disposables <- []

        static member Capture () = 
            let stringWriter = new System.IO.StringWriter ()
            let redirector = new Redirector (stringWriter)
            lazy(
                dispose redirector
                dispose stringWriter
                stringWriter.Current
            )

        interface IDisposable with
            member x.Dispose () = x.Revert ()

    let InjectNewlinePrefix (prefix: string) = 
        let previousOut = Console.Out
        let newOut, cleanup = IO.NewlinePrefixInjectorWriter prefix (previousOut.Encoding, previousOut)
        do Console.SetOut ( newOut )
        IDisposable.Create (runOnce (fun () ->  
            dispose cleanup
            Console.SetOut previousOut
        ))
        
    let InjectIndent (count: int) = 
        InjectNewlinePrefix (String(' ', count))

    let BoxText (width: int) (padding: int) (wrapindent: int) = 
        let previousOut = Console.Out
        
        let prefix = "|" + (String (' ', padding))
        let suffix = (String (' ', max 1 (padding / 2))) + "|"
        let wrapPrefix = "|" + (String (' ', padding - 1)) + "\\" + (String (' ', wrapindent))
        previousOut.Write "."
        previousOut.Write (String ('-', max 0 (width - 2)))
        previousOut.Write "."
        previousOut.WriteLine ()
        let offset = ref 0

        let rec writeInline (s: string) = 
            if s.Length > 0 then
                if !offset = 0 then
                    previousOut.Write prefix
                    offset := prefix.Length
                let remaining = width - suffix.Length - !offset
                if remaining >= s.Length then
                    previousOut.Write s
                    offset := !offset + s.Length
                else
                    previousOut.Write (s.[0 .. remaining - 1])
                    previousOut.WriteLine suffix
                    previousOut.Write wrapPrefix
                    offset := wrapPrefix.Length
                    writeInline s.[remaining .. ]

        let endline () = 
            if !offset = 0 then 
                previousOut.Write prefix
                previousOut.Write (String(' ', width - prefix.Length - suffix.Length))
                previousOut.WriteLine suffix
            else
                let remaining = width - suffix.Length - !offset
                if remaining > 0 then
                    previousOut.Write (String(' ', remaining))
                else
                    assert(remaining = 0)
                previousOut.WriteLine suffix
                offset := 0

        let write (s: string) =
            let lines = s.GetLines ()
            lines |> Array.iteri (fun linei line ->
                writeInline line
                if lines.Length > 0 && linei < lines.Length - 1 then
                    endline ()
            )

        let newOut =
            IO.stringCallbackTextWriter (previousOut.Encoding, write)

        do Console.SetOut ( newOut )
        IDisposable.Create (runOnce (fun () ->  
            if !offset > 0 then
                endline ()
            previousOut.WriteLine (sprintf "`%s'" (String ('-', max 0 (width - 2))))
            Console.SetOut previousOut
        ))

module Text = 
    let inline Append (s : string) (sb : System.Text.StringBuilder) = sb.Append s

    module RegularExpressions = 
        open System.Text.RegularExpressions

        let tryMatch (regex : Regex) (s : string) = 
            let m = regex.Match s
            if m.Success
            then
                Some m.Groups
            else
                None

        let getValues (gc : GroupCollection) = 
            Array.init (gc.Count) (fun i -> gc.[i].Value)

        type Regex with
            member inline regex.TryMatch s = tryMatch regex s

        type GroupCollection with
            member inline gc.Values = getValues gc