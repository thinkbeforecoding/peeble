module Tests

open System
open Xunit
open Peeble
open FSharp.Compiler.SourceCodeServices
open Peeble.Program
open Fable
open Peeble.PhpAst
open Peeble.Transforms
open System.IO
open Swensen.Unquote

let compile input = 
    let opts   =
        let projOptions: FSharpProjectOptions =
                 {
                     ProjectId = None
                     ProjectFileName = "test.fsproj"
                     SourceFiles = [|"test.fs"|]
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


    let checker = InteractiveChecker.Create(opts)
    let result = checker.ParseAndCheckProject( "test.fsproj", [|"test.fs"|], (fun n -> 0, lazy input)) 
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
            [ for file in ["test.fs"] do
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

        { Decls = fs }

let write file =
    let w = new StringWriter()
    let ctx = PhpOutput.Writer.create(w, emitPhpMark = false, emitDeclNumber = false)
    PhpOutput.writeFile ctx file
    (string w).Trim([|'\r';'\n'|])
    

let equals (expected: 'a) (actual: 'a) =
    Assert.Equal<'a>(expected, actual)

[<Fact>]
let ``Test global declaration expression`` () =
    let result = 
        """let x = 0"""
        |> compile
        |> write
    test <@ result = """$GLOBALS['Test___x'] = 0;""" @>
    
[<Fact>]
let ``Test global declaration expression tree`` () =
    let result = 
        """let x = 0"""
        |> compile
    test <@ result.Decls = [ 0, PhpDeclValue("Test___x", PhpConst(PhpConstNumber 0.)) ] @>
  

    
