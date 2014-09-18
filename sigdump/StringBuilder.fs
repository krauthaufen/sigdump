#if INTERACTIVE
#r "..\\packages\\Mono.Cecil.0.9.5.4\\lib\\net40\\Mono.Cecil.dll"
#else
namespace Sigdump
#endif

open System
open System.Text
open Mono.Cecil

[<AutoOpen>]
module StringBuilder =

    let private lineRx = System.Text.RegularExpressions.Regex @"\r\n"

    type IStringWriter =
        abstract member Write : int * string * [<ParamArray>] args : obj[] -> unit
        abstract member Flush : unit -> unit

    type StringBuilderWriter() =
        let b = StringBuilder()

        member x.StringValue =
            b.ToString()

        member x.Clear() =
            b.Clear() |> ignore

        interface IStringWriter with
            member x.Write(indent, str, args) =
                if indent <> 0 then
                    
                    let str = System.String.Format(str, args)
                    let elements = lineRx.Split str
                    if elements.Length > 1 then
                        let str = elements |> Array.map (fun e -> (new System.String(' ', indent * 4)) + e) |> String.concat "\r\n"
                        b.Append(str) |> ignore
                    else
                        b.Append(str) |> ignore
                else
                    b.AppendFormat(str, args) |> ignore
            member x.Flush() = ()

    type StreamWriterBuilder() =
        let ms = new System.IO.MemoryStream()
        let w = new System.IO.StreamWriter(ms)

        member x.Data =
            w.Flush()
            ms.ToArray()

        member x.Dispose() =
            ms.Dispose()

        interface IStringWriter with
            member x.Write(indent, str, args) =
                w.Write(str, args) |> ignore
            member x.Flush() = ()

        interface System.IDisposable with
            member x.Dispose() = x.Dispose()



    type ConsoleWriter() =
        interface IStringWriter with
            member x.Write(indent, str,args) =
                if indent <> 0 then
                    let str = System.String.Format(str, args)
                    let elements = lineRx.Split str
                    if elements.Length > 1 then
                        let str = elements |> Array.map (fun e -> (new System.String(' ', indent * 4)) + e) |> String.concat "\r\n"
                        Console.Write("{0}", str) |> ignore
                    else
                        Console.Write("{0}", str)
                else
                    Console.Write(str,args)

            member x.Flush() = 
                Console.Out.Flush()
                Console.Out.Flush()




    type StringBuilderState = { writer : IStringWriter; indentation : int }
    type StringBuilder<'a> = { run : StringBuilderState -> StringBuilderState * 'a }

    let read (m : StringBuilder<'a>) =
        { run = fun s ->
            let childWriter = StringBuilderWriter()
            let (s', v) = m.run { s with writer = childWriter }
            let str = childWriter.StringValue
            childWriter.Clear()

            (s, (v, str))
        }

    let readString (m : StringBuilder<'a>) =
        { run = fun s ->
            let childWriter = StringBuilderWriter()
            let (s', _) = m.run { s with writer = childWriter }
            let str = childWriter.StringValue
            childWriter.Clear()

            (s, str)
        }

    let getState =
        { run = fun s -> (s,s) }

    let putState s =
        { run = fun _ -> (s,()) }

    let modifyState f =
        { run = fun s -> (f s, ()) }

    let pushIndent =
        modifyState (fun s -> { s with indentation = s.indentation + 1 })

    let popIndent =
        modifyState (fun s -> { s with indentation = s.indentation - 1 })


    let runState (s : StringBuilderState) (e : StringBuilder<'a>) =
        let (_, r) = e.run s
        s.writer.Flush()
        r

    let runWriter (w : IStringWriter) (e : StringBuilder<'a>) =
        runState { writer = w; indentation = 0 } e

    let toString (e : StringBuilder<unit>) =
        let w = StringBuilderWriter()
        runWriter w e
        w.StringValue

    let toByteArray (e : StringBuilder<unit>) =
        let w = new StreamWriterBuilder()
        runWriter w e
        let r = w.Data
        w.Dispose()
        r

    type StringWriterBuilder() =

        member x.Bind(m : StringBuilder<'a>, f : 'a -> StringBuilder<'b>) : StringBuilder<'b> =
            { run = fun s ->
                let (s', v) = m.run s
                (f v).run s'
            }

        member x.Bind(m : ref<'a>, f : ref<'a> -> StringBuilder<'b>) : StringBuilder<'b> =
            let v = !m
            { run = fun s ->
                let copyRef = ref v
                (f copyRef).run s
            }

        member x.Return(v : 'a) =
            { run = fun s -> s,v }

        member x.For(seq : seq<'a>, f : 'a -> StringBuilder<unit>) : StringBuilder<unit> =
            { run = fun s ->
                let mutable c = s
                for e in seq do
                    let (s', ()) = (f e).run c
                    c <- s'

                (c, ())
            }

        member x.Delay (f : unit -> StringBuilder<'a>) =
            { run = fun s -> (f ()).run s }

        member x.Zero() = 
            { run = fun s -> s, () }

        member x.Yield (str : string) =
            { run = fun s -> s, s.writer.Write(s.indentation, "{0}", str) }

        member x.YieldFrom (str : StringBuilder<'a>) =
            str


        member x.Combine(l : StringBuilder<unit>, r : StringBuilder<'a>) =
            { run = fun s -> 
                let (s', ()) = l.run s
                r.run s'
            }

    let fmt (fmt : string) (args : #seq<obj>) =
        { run = fun s ->
            s.writer.Write(s.indentation, fmt, args |> Seq.toArray)
            s,()
        }

    let str = StringWriterBuilder()



    let test() =
        let s = [1;2;3;4]

        let someThingElse =
            str {
                yield "%%%"
            }

        str {
            let! c = readString someThingElse
            
            for e in s do
                
                yield c
                yield e.ToString()

            yield! fmt "{0}" [obj()]

            yield "asdsad"

            yield "\r\n"

        } |> runWriter (ConsoleWriter())