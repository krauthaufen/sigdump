#if INTERACTIVE
#r "..\\packages\\Mono.Cecil.0.9.5.4\\lib\\net40\\Mono.Cecil.dll"
#r ".\\bin\\Debug\\sigdump.exe"
open Sigdump
#else
namespace Sigdump
#endif

open System
open System.Text
open Mono.Cecil

[<AutoOpen>]
module Sigdump =
    let private genRx = System.Text.RegularExpressions.Regex @"\`[0-9]+"
    let intrinsicName (t : TypeReference) =
        match t.FullName with
            | "System.Void" -> Some "void"
            | "System.Boolean" -> Some "bool"
            | "System.SByte" -> Some "sbyte"
            | "System.Int16" -> Some "short"
            | "System.Int32" -> Some "int"
            | "System.Int64" -> Some "long"
            | "System.Byte" -> Some "byte"
            | "System.UInt16" -> Some "ushort"
            | "System.UInt32" -> Some "uint"
            | "System.UInt64" -> Some "ulong"
            | "System.Single" -> Some "float"
            | "System.Double" -> Some "double"
            | "System.Decimal" -> Some "decimal"
            | n -> None

    let private fullTypeName (t : TypeReference) =
        match intrinsicName t with
            | Some i -> i
            | None -> genRx.Replace(t.FullName, "")

    let private typeName (t : TypeReference) =
        match intrinsicName t with
            | Some i -> i
            | None -> genRx.Replace(t.FullName, "")


    let rec private writeType (t : TypeDefinition) =
        str {
            if t.IsPublic || t.IsNestedPublic then

                let isStruct = t.BaseType <> null && t.BaseType.FullName = "System.ValueType"
                let isAttributeType = t.BaseType <> null && t.BaseType.FullName = "System.Attribute"

                if isAttributeType then
                    let! ac = ref 0
                    for att in t.CustomAttributes do
                        yield! fmt "[{0}(" [fullTypeName att.AttributeType]

                        let! c = ref 0
                        for a in att.ConstructorArguments do
                            if !c > 0 then yield ", "
                            yield a.Value.ToString()
                            c := !c + 1

                        for a in att.Fields do
                            if !c > 0 then yield ", "
                            yield! fmt "{0} = {1}" [a.Name; a.Argument.Value.ToString()]
                            c := !c + 1

                        for a in att.Properties do
                            if !c > 0 then yield ", "
                            yield! fmt "{0} = {1}" [a.Name; a.Argument.Value.ToString()]
                            c := !c + 1

                        yield ")]"

                        yield "\r\n"
                        ac := !ac + 1

                yield "public "
            

                if t.IsClass && not isStruct && not t.IsEnum then
                    if t.IsSealed && t.IsAbstract then
                        yield "static "
                    elif t.IsSealed then
                        yield "sealed "
                    elif t.IsAbstract then
                        yield "abstract "

                if t.IsEnum then
                    yield "enum "
                elif isStruct then
                    yield "struct "
                elif t.IsInterface then
                    yield "interface "
                else
                    yield "class "

                yield genRx.Replace(t.Name, "")

                if t.HasGenericParameters then
                    let str = t.GenericParameters |> Seq.map (fun p -> p.Name) |> String.concat ", "
                    yield! fmt "<{0}>" [str]

                let! hasBaseType = ref false

                if t.BaseType <> null && t.BaseType.FullName <> "System.Object" && not t.IsEnum && not isStruct then
                    hasBaseType := true
                    yield! fmt " : {0}" [fullTypeName t.BaseType]

                if t.HasInterfaces then
                    let! c = ref 0
                    for i in t.Interfaces do
                        if !c = 0 && not !hasBaseType then
                            yield " : "
                        else
                            yield ", "
                        c := !c + 1

                        if i.Namespace <> t.Namespace then
                            yield fullTypeName i
                        else
                            yield typeName i

                yield "\r\n{"
                do! pushIndent
                yield "\r\n"

                for n in t.NestedTypes do
                    yield! writeType n

                let! cnt = ref 0
                for f in t.Fields do
                    if f.IsPublic && (not t.IsEnum || f.Name <> "value__") then
                        if !cnt > 0 then
                            yield "\r\n"

                        yield "public "

                        if f.IsStatic then
                            yield "static "

                        if f.FieldType.Namespace = t.Namespace then
                            yield! fmt "{0} {1};" [typeName f.FieldType; f.Name]
                        else
                            yield! fmt "{0} {1};" [fullTypeName f.FieldType; f.Name]
                        cnt := !cnt + 1
                        

                let! cnt = ref 0
                for m in t.Methods do
                    if m.IsPublic && not m.IsGetter && not m.IsSetter then
                        if !cnt > 0 then
                            yield "\r\n"
                        cnt := !cnt + 1
                        yield "public "
                        
                        let isExtension = m.IsStatic && m.CustomAttributes |> Seq.exists (fun a -> a.AttributeType.FullName = "System.Runtime.CompilerServices.ExtensionAttribute")

                        if m.IsConstructor then
                            yield typeName t
                        else

                            if m.IsStatic then
                                yield "static "

                            
                            if m.ReturnType.Namespace = t.Namespace then
                                yield! fmt "{0} " [typeName m.ReturnType]
                            else
                                yield! fmt "{0} " [fullTypeName m.ReturnType]

                            yield m.Name

                            if m.HasGenericParameters then
                                let str = m.GenericParameters |> Seq.map (fun p -> p.Name) |> String.concat ", "
                                yield! fmt "<{0}>" [str]
    
                        yield "("
                        let! pi = ref 0
                        for p in m.Parameters do
                            if !pi > 0 then
                                yield ", "

                            if isExtension && !pi = 0 then
                                yield "this "

                            yield! fmt "{0} arg{1}" [p.ParameterType |> fullTypeName; !pi]

                            pi := !pi + 1
                        yield "){ /* ... */ }"

                for p in t.Properties do
                    let getterPublic = p.GetMethod <> null && p.GetMethod.IsPublic
                    let setterPublic = p.SetMethod <> null && p.SetMethod.IsPublic
                    let getterStatic = p.GetMethod <> null && p.GetMethod.IsStatic
                    let setterStatic = p.SetMethod <> null && p.SetMethod.IsStatic

                    if getterPublic || setterPublic then

                        yield "\r\n"
                        yield "public "

                        if getterStatic || setterStatic then
                            yield "static "

                        if p.PropertyType.Namespace = t.Namespace then
                            yield! fmt "{0} " [typeName p.PropertyType]
                        else
                            yield! fmt "{0} " [fullTypeName p.PropertyType]

                        yield p.Name
                        yield " { "

                        if getterPublic then
                            yield "get; "
                        if setterPublic then
                            yield "set; "

                        yield "}"


                do! popIndent

                yield "\r\n}\r\n"


        }

    let private writeAssemby(a : AssemblyDefinition) =
        str {

            for (ns,types) in a.MainModule.Types |> Seq.groupBy (fun t -> t.Namespace) do
                if ns <> null && ns <> "" then
                    yield! fmt "namespace {0}\r\n{{" [ns]

                    do! pushIndent
                    yield "\r\n"

                    for t in types do
                        yield! writeType t

                    do! popIndent

                    yield "\r\n}\r\n"

        }

    let private sha512 = System.Security.Cryptography.SHA512.Create()

    let computeHash (assPath : string) =
        let ass = AssemblyDefinition.ReadAssembly assPath
        let data = ass |> writeAssemby |> toByteArray
        let hash = sha512.ComputeHash data
        hash |> System.Convert.ToBase64String

    let toString (assPath : string) =
        let ass = AssemblyDefinition.ReadAssembly assPath
        ass |> writeAssemby |> toString

    let test() =
        let ass =  @"E:\Development\TestDll\TestDll\bin\Debug\TestDll.exe"
        let hash = computeHash ass

        printfn "%A" hash