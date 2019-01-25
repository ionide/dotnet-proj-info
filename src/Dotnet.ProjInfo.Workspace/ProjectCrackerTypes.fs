namespace Dotnet.ProjInfo.Workspace

open System
open System.IO

type FilePath = string

[<RequireQualifiedAccess>]
type ProjectSdkType =
    | Verbose of ProjectSdkTypeVerbose
    | DotnetSdk of ProjectSdkTypeDotnetSdk
and ProjectSdkTypeVerbose =
    {
      TargetPath: string
      TargetFrameworkVersion: string
      Configuration: string
    }
and ProjectSdkTypeDotnetSdk =
    {
      IsTestProject: bool
      Configuration: string // Debug
      IsPackable: bool // true
      TargetFramework: string // netcoreapp1.0
      TargetFrameworkIdentifier: string // .NETCoreApp
      TargetFrameworkVersion: string // v1.0

      MSBuildAllProjects: FilePath list //;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.SupportedTargetFrameworks.props;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.BeforeCommon.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultOutputPaths.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.TargetFrameworkInference.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.RuntimeIdentifierInference.targets;C:\Users\e.sada\.nuget\packages\fsharp.net.sdk\1.0.5\build\FSharp.NET.Core.Sdk.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\c1.fsproj;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.Common.CurrentVersion.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\NuGet.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\15.0\Microsoft.Common.targets\ImportAfter\Microsoft.TestPlatform.ImportAfter.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.TestPlatform.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.proj-info.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.Common.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.PackageDependencyResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DisableStandardFrameworkResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.GenerateAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Publish.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.PreserveCompilationContext.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\NuGet.Build.Tasks.Pack\build\NuGet.Build.Tasks.Pack.targets
      MSBuildToolsVersion: string // 15.0

      ProjectAssetsFile: FilePath // e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\project.assets.json
      RestoreSuccess: bool // True

      Configurations: string list // Debug;Release
      TargetFrameworks: string list // netcoreapp1.0;netstandard1.6

      TargetPath: string

      //may not exists
      RunArguments: string option // exec "e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\bin\Debug\netcoreapp1.0\c1.dll"
      RunCommand: string option // dotnet

      //from 2.0
      IsPublishable: bool option // true

    }

type ExtraProjectInfoData =
    {
        ProjectOutputType: ProjectOutputType
        ProjectSdkType: ProjectSdkType
    }
and ProjectOutputType =
    | Library
    | Exe
    | Custom of string


type GetProjectOptionsErrors =
     | ProjectNotRestored of string
     | GenericError of string * string


type ProjectOptions =
    {
        ProjectId: string option
        ProjectFileName: string
        OtherOptions: string list
        ReferencedProjects: (string * ProjectOptions) list
        LoadTime: DateTime
        ExtraProjectInfo: ExtraProjectInfoData
    }

type [<RequireQualifiedAccess>] WorkspaceProjectState =
    | Loading of string * ((string * string) list)
    | Loaded of ProjectOptions * string list * Map<string,string>
    | Failed of string * GetProjectOptionsErrors

module ProjectRecognizer =

    let (|NetCoreProjectJson|NetCoreSdk|Net45|Unsupported|) file =
        //.NET Core Sdk preview3+ replace project.json with fsproj
        //Easy way to detect new fsproj is to check the msbuild version of .fsproj
        //Post preview5 has (`Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk"`), use that
        //  for checking .NET Core fsproj. NB: casing of FSharp may be inconsistent.
        //The `dotnet-compile-fsc.rsp` are created also in `preview3+`, so we can
        //  reuse the same behaviour of `preview2`
        let rec getProjectType (sr:StreamReader) limit =
            // post preview5 dropped this, check Sdk field
            let isNetCore (line:string) = line.ToLower().Contains("sdk=")
            if limit = 0 then
                Unsupported // unsupported project type
            else
                let line = sr.ReadLine()
                if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                    getProjectType sr (limit-1)
                else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                    if isNetCore line then NetCoreSdk else Net45
        if Path.GetExtension file = ".json" then
            NetCoreProjectJson // dotnet core preview 2 or earlier
        else
            use sr = File.OpenText(file)
            getProjectType sr 3


module FscArguments =

  open CommonHelpers

  let outType rsp =
      match List.tryPick (chooseByPrefix "--target:") rsp with
      | Some "library" -> ProjectOutputType.Library
      | Some "exe" -> ProjectOutputType.Exe
      | Some v -> ProjectOutputType.Custom v
      | None -> ProjectOutputType.Exe // default if arg is not passed to fsc

  let private outputFileArg = ["--out:"; "-o:"]

  let private makeAbs projDir f =
      if Path.IsPathRooted f then f else Path.Combine(projDir, f)

  let outputFile projDir rsp =
      rsp
      |> List.tryPick (chooseByPrefix2 outputFileArg)
      |> Option.map (makeAbs projDir)

  let isCompileFile (s:string) =
      s.EndsWith(".fs") || s.EndsWith (".fsi")

  let compileFiles =
      //TODO filter the one without initial -
      List.filter isCompileFile

  let references =
      //TODO valid also --reference:
      List.choose (chooseByPrefix "-r:")

  let useFullPaths projDir (s: string) =
    match s |> splitByPrefix2 outputFileArg with
    | Some (prefix, v) ->
        prefix + (v |> makeAbs projDir)
    | None ->
        if isCompileFile s then
            s |> makeAbs projDir |> Path.GetFullPath
        else
            s
