#r "paket:
nuget Fake.Core
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Testing.Coverlet
nuget Fake.Testing.ReportGenerator
"
#load ".fake/build.fsx/intellisense.fsx"


open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.Testing


module Path =
    let root = __SOURCE_DIRECTORY__
    let bin = root </> "bin"
    let peeble = bin </> "peeble"
    let tests = bin </> "tests"
    let coverage = tests </> "coverage.json"

Target.create "Clean" <| fun _ ->
    Directory.delete Path.bin

Target.create "Build" <| fun _ ->
    Fake.DotNet.DotNet.publish (fun p ->
        { p with Configuration = Fake.DotNet.DotNet.BuildConfiguration.Release
                 OutputPath = Some Path.peeble }
                 
    ) "src/peeble/peeble.fsproj"

Target.create "Test" <| fun _ ->
    Fake.DotNet.DotNet.test (fun p ->
        { p with 
            // Your dotnet test configuration here...
            Configuration = Fake.DotNet.DotNet.BuildConfiguration.Release
        }
        |> Fake.DotNet.Testing.Coverlet.withDotNetTestOptions (fun p ->
            { p with
                Output = Path.coverage
                OutputFormat = Coverlet.OpenCover
                Include = [ "peeble", "Peeble.*" ]
            }))
        "tests/peeble.tests/peeble.tests.fsproj"

    ReportGenerator.generateReports (fun p ->
            { p with
                ToolType = Fake.DotNet.ToolType.CreateLocalTool()
                TargetDir = Path.tests
                ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.Badges ]
            })
            [ Path.coverage ]

Target.create "All" ignore


"Clean" ==> "Build"
        ==> "Test"
        ==> "All"

Target.runOrDefault "All"





