module Peeble.Program

open System
open Fable
open FSharp.Compiler.SourceCodeServices
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO

open Fable.AST
open Peeble.PhpAst
open Peeble.Transforms

type Project(projectOptions: FSharpProjectOptions,
                implFiles: IDictionary<string, FSharpImplementationFileContents>,
                errors: FSharpErrorInfo array) =
    let projectFile = Path.normalizePath projectOptions.ProjectFileName
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    //let rootModules =
    //    implFiles |> Seq.map (fun kv ->
    //        kv.Key, FSharp2Fable.Compiler.getRootModuleFullName kv.Value) |> dict
    member __.ImplementationFiles = implFiles
    member __.RootModules = dict [] //rootModules
    member __.InlineExprs = inlineExprs
    member __.Errors = errors
    member __.ProjectOptions = projectOptions
    member __.ProjectFile = projectFile
    member __.GetOrAddInlineExpr(fullName, generate) =
        inlineExprs.GetOrAdd(fullName, fun _ -> generate())


type Log =
    { Message: string
      Tag: string
      Severity: Severity
      Range: SourceLocation option
      FileName: string option }

type Compiler(currentFile, project: Project, options, fableLibraryDir: string) =
    let mutable id = 0
    let logs = ResizeArray<Log>()
    let fableLibraryDir = fableLibraryDir.TrimEnd('/')
    member __.GetLogs() =
        logs |> Seq.toList
    member __.GetFormattedLogs() =
        let severityToString = function
            | Severity.Warning -> "warning"
            | Severity.Error -> "error"
            | Severity.Info -> "info"
        logs
        |> Seq.groupBy (fun log -> severityToString log.Severity)
        |> Seq.map (fun (severity, logs) ->
            logs |> Seq.map (fun log ->
                match log.FileName with
                | Some file ->
                    match log.Range with
                    | Some r -> sprintf "%s(%i,%i): (%i,%i) %s %s: %s" file r.start.line r.start.column r.``end``.line r.``end``.column severity log.Tag log.Message
                    | None -> sprintf "%s(1,1): %s %s: %s" file severity log.Tag log.Message
                | None -> log.Message)
            |> Seq.toArray
            |> Tuple.make2 severity)
        |> Map
    member __.Options = options
    member __.CurrentFile = currentFile
    interface ICompiler with
        member __.Options = options
        member __.LibraryDir = fableLibraryDir
        member __.CurrentFile = currentFile
        member x.GetRootModule(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match project.RootModules.TryGetValue(fileName) with
            | true, rootModule -> rootModule
            | false, _ ->
                let msg = sprintf "Cannot find root module for %s. If this belongs to a package, make sure it includes the source files." fileName
                (x :> ICompiler).AddLog(msg, Severity.Warning)
                "" // failwith msg
        member __.GetOrAddInlineExpr(fullName, generate) =
            project.InlineExprs.GetOrAdd(fullName, fun _ -> generate())
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            { Message = msg
              Tag = defaultArg tag "FABLE"
              Severity = severity
              Range = range
              FileName = fileName }
            |> logs.Add
        // TODO: If name includes `$$2` at the end, remove it
        member __.GetUniqueVar(name) =
            id <- id + 1
            Naming.getUniqueName (defaultArg name "var") id







open System.Xml.Linq


[<EntryPoint>]
let main argv =

    match argv with
    | [| project |] ->
        let dir = Path.GetDirectoryName project

        let proj = XDocument.Load(project)
        let files =
            proj.Descendants(XName.Get "Compile")
            |> Seq.map (fun e -> e.Attribute(XName.Get "Include").Value)
            |> Seq.map (fun p -> Path.Combine(dir, p))
            |> Seq.toList

        let opts   =
            let projOptions: FSharpProjectOptions =
                     {
                         ProjectId = None
                         ProjectFileName = project
                         SourceFiles = List.toArray files 
                         OtherOptions = [| @"-r:Fable.Core.dll"|]
                         ReferencedProjects = [||] //p2pProjects |> Array.ofList
                         IsIncompleteTypeCheckEnvironment = false
                         UseScriptResolutionRules = false
                         LoadTime = DateTime.Now
                         UnresolvedReferences = None;
                         OriginalLoadReferences = []
                         ExtraProjectInfo = None
                         Stamp = None
                     }
            projOptions


        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let result = checker.ParseAndCheckProject(opts) |> Async.RunSynchronously
        match result.Errors with
        | [||] ->
            let impls =
                [ for imp in result.AssemblyContents.ImplementationFiles do 
                    imp.FileName, imp
                    ]
                |> dict

            let proj = Project(opts ,impls,[||])
            let compOptions =
                { CompilerOptions.typedArrays = false
                  CompilerOptions.clampByteArrays = false
                  CompilerOptions.debugMode = false
                  CompilerOptions.outputPublicInlinedFunctions = false
                  CompilerOptions.precompiledLib = None
                  CompilerOptions.verbosity = Verbosity.Normal
                  CompilerOptions.classTypes = true
                  CompilerOptions.typescript = false
                   }

            let asts =
                [ for file in files do
                    let com = Compiler(file, proj, compOptions, "")
                    Fable.Transforms.FSharp2Fable.Compiler.transformFile com proj.ImplementationFiles
                    |> Fable.Transforms.FableTransforms.optimizeFile com ]

            let phpComp = PhpCompiler.empty
            let fs = 
                [ 
                  for ast in asts do
                      for i,decl in List.indexed ast.Declarations do
                        for d in convertDecl phpComp decl do
                            i,d
                ]

            let w = new StringWriter()
            let ctx = PhpOutput.Writer.create(w)
            let file = { Decls = fs }
            PhpOutput.writeFile ctx file
            printfn "%s" (string w)
        | errors ->
            for error in errors do
                eprintfn "[%s %d %d] %s" error.FileName error.Start.Line error.Start.Column error.Message
    | _ -> eprintfn "You should provide a project"

    // IO.File.WriteAllText(@"lib.php", string w)
            
    0 // return an integer exit code
