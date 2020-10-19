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
open Argu

module CmdLine =
    type Options =
        | [<Mandatory; MainCommand>] Project of string
        | Output of string

        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Project _ -> "fsproj Project to transpile"
                | Output _ -> "The output php file"


let private runProcess (workingDir: string) (exePath: string) (args: string) =
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi

    let sbOut = System.Text.StringBuilder()
    p.OutputDataReceived.Add(fun ea -> sbOut.AppendLine(ea.Data) |> ignore)

    let sbErr = System.Text.StringBuilder()
    p.ErrorDataReceived.Add(fun ea -> sbErr.AppendLine(ea.Data) |> ignore)

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode
    exitCode, (workingDir, exePath, args)
module Process =
    let private runProcess (workingDir: string) (exePath: string) (args: string) =
        let logOut = System.Collections.Concurrent.ConcurrentQueue<string>()
        let logErr = System.Collections.Concurrent.ConcurrentQueue<string>()

        let psi = System.Diagnostics.ProcessStartInfo()
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.Arguments <- args
        psi.CreateNoWindow <- true
        psi.UseShellExecute <- false

        use p = new System.Diagnostics.Process()
        p.StartInfo <- psi

        p.OutputDataReceived.Add(fun ea -> logOut.Enqueue (ea.Data))
        p.ErrorDataReceived.Add(fun ea -> logErr.Enqueue (ea.Data))

        let exitCode =
            try
                p.Start() |> ignore
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                p.ExitCode
            with ex ->
                logErr.Enqueue ("Cannot run: " + ex.Message)
                -1

        exitCode, logOut.ToArray(), logErr.ToArray()

    // Adapted from https://github.com/enricosada/dotnet-proj-info/blob/1e6d0521f7f333df7eff3148465f7df6191e0201/src/dotnet-proj/Program.fs#L155
    let runCmd log workingDir exePath args =
        log (workingDir + "> " + exePath + " " + (args |> String.concat " "))

        let exitCode, logOut, logErr =
            String.concat " " args
            |> runProcess workingDir exePath

        Array.append logOut logErr
        |> String.concat "\n"
        |> log

        exitCode

let getProjectFscArgs dir project =
    let getFscArgs = Dotnet.ProjInfo.Inspect.getFscArgs
    let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
    let gp () = Dotnet.ProjInfo.Inspect.getProperties (["TargetPath"; "IsCrossTargetingBuild"; "TargetFrameworks"; "TargetFramework"])
     
    let results =
        let runCmd exePath args = runProcess dir exePath (args |> String.concat " ")

        let msbuildExec = Dotnet.ProjInfo.Inspect.dotnetMsbuild runCmd
        let log  = ignore

        let additionalArgs = [] //additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

        project
        |> Dotnet.ProjInfo.Inspect.getProjectInfos log msbuildExec [getFscArgs; getP2PRefs; gp] additionalArgs

    match results with
    | Ok (Ok (Dotnet.ProjInfo.Inspect.FscArgs r) :: _) -> r
    | Error e -> failwithf "%A" e


[<EntryPoint>]
let main argv =

    let parser = Argu.ArgumentParser.Create<CmdLine.Options>("peeble")
    let parseResult = parser.ParseCommandLine(argv) 

    match parseResult.TryGetResult( CmdLine.Project ) with
    | Some project ->
        let project = 
            if Path.IsPathRooted(project : string) then
                project
            else
                Path.Combine(Environment.CurrentDirectory, project)

        let dir = Path.GetDirectoryName project

        let proj = XDocument.Load(project)
        let files =
            proj.Descendants(XName.Get "Compile")
            |> Seq.map (fun e -> e.Attribute(XName.Get "Include").Value)
            |> Seq.map (fun p -> Path.Combine(dir, p))
            |> Seq.toList

        Process.runCmd ignore
            dir
            "dotnet" ["restore"; IO.Path.GetFileName project]
        |> ignore


        let fscopts = getProjectFscArgs dir project

        let opts   =
            let projOptions: FSharpProjectOptions =
                     {
                         ProjectId = None
                         ProjectFileName = project
                         SourceFiles = List.toArray files 
                         OtherOptions = Array.ofList fscopts //   [| "--targetprofile:netstandard" (*; @"-r:Fable.Core.dll"*)|]
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

            match parseResult.TryGetResult(CmdLine.Output) with
            | Some file -> 
                use w = File.CreateText file
                let ctx = PhpOutput.Writer.create(w)
                let file = { Decls = fs }
                PhpOutput.writeFile ctx file



            | None ->
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
