namespace Dotnet.ProjInfo.Workspace

open System.Collections.Concurrent
open ProjectRecognizer
open System.IO

type ProjectKey =
    { ProjectPath: string
      Configuration: string
      TargetFramework: string }

type Loader () =

    let event1 = new Event<_>()
    let parsedProjects = ConcurrentDictionary<_, _>()

    let mutable msbuildPath = Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"
    let mutable msbuildNetSdkPath = Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"

    let getKey (po: ProjectOptions) =
        { ProjectKey.ProjectPath = po.ProjectFileName
          Configuration =
            match po.ExtraProjectInfo.ProjectSdkType with
            | ProjectSdkType.DotnetSdk t ->
                t.Configuration
            | ProjectSdkType.Verbose v ->
                v.Configuration
          TargetFramework =
            match po.ExtraProjectInfo.ProjectSdkType with
            | ProjectSdkType.DotnetSdk t ->
                t.TargetFramework
            | ProjectSdkType.Verbose v ->
                v.TargetFrameworkVersion |> Dotnet.ProjInfo.NETFramework.netifyTargetFrameworkVersion
        }

    [<CLIEvent>]
    member __.Notifications = event1.Publish

    member __.Projects
        with get () = parsedProjects.ToArray()

    // TODO get only
    member this.MSBuildPath
        with get () = msbuildPath
        and set (value) = msbuildPath <- value

    // TODO get only
    member this.MSBuildNetSdkPath
        with get () = msbuildNetSdkPath
        and set (value) = msbuildNetSdkPath <- value

    member this.LoadProjects(projects: string list) =
        let cache = ProjectCrackerDotnetSdk.ParsedProjectCache()
        
        let notify arg =
            event1.Trigger(this, arg)

        for project in projects do

            let loader =
                if File.Exists project then
                    match project with
                    | NetCoreSdk ->
                        ProjectCrackerDotnetSdk.load this.MSBuildNetSdkPath
                    | Net45 ->
                        ProjectCrackerDotnetSdk.loadVerboseSdk this.MSBuildPath
                    | NetCoreProjectJson | Unsupported ->
                        failwithf "unsupported project %s" project
                 else
                    fun notify _ proj ->
                        let loading = WorkspaceProjectState.Loading (proj, [])
                        notify loading
                        Error (GetProjectOptionsErrors.GenericError(proj, "not found"))

            match loader notify cache project with
            | Ok (po, sources, props) ->
                // TODO sources and props are wrong, because not project specific. but of root proj
                let loaded po = WorkspaceProjectState.Loaded (po, sources, props)

                let rec visit (p: ProjectOptions) = seq {
                    yield p
                    for (_, p2p) in p.ReferencedProjects do
                        yield! visit p2p }

                for proj in visit po do
                    parsedProjects.AddOrUpdate(getKey proj, proj, fun _ _ -> proj) |> ignore

                for proj in visit po do
                    notify (loaded proj)

            | Error e ->
                let failed = WorkspaceProjectState.Failed (project, e)
                notify failed

    member this.LoadSln(slnPath: string) =

        match InspectSln.tryParseSln slnPath with
        | Choice1Of2 (_, slnData) ->
            let projs = InspectSln.loadingBuildOrder slnData

            this.LoadProjects(projs)
        | Choice2Of2 d ->
            failwithf "cannot load the sln: %A" d


type NetFWInfo () =

    let mutable msbuildPath = Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"

    let installedNETVersionsLazy = lazy (NETFrameworkInfoProvider.getInstalledNETVersions msbuildPath)

    let additionalArgsByTfm = System.Collections.Concurrent.ConcurrentDictionary<string, string list>()

    let additionalArgumentsBy targetFramework =
        let f tfm = NETFrameworkInfoProvider.getAdditionalArgumentsBy msbuildPath tfm
        additionalArgsByTfm.GetOrAdd(targetFramework, f)

    // TODO get only
    member this.MSBuildPath
        with get () = msbuildPath
        and set (value) = msbuildPath <- value

    member this.InstalledNetFws() =
        installedNETVersionsLazy.Force()

    member this.LatestVersion () =
        let maxByVersion list =
            //TODO extract and test
            list
            |> List.map (fun (s: string) -> s, (s.TrimStart('v').Replace(".","").PadRight(3, '0')))
            |> List.maxBy snd
            |> fst

        this.InstalledNetFws()
        |> maxByVersion

    member this.GetProjectOptionsFromScript(checkerGetProjectOptionsFromScript, targetFramework, file, source) =
        FSharpCompilerServiceChecker.getProjectOptionsFromScript additionalArgumentsBy checkerGetProjectOptionsFromScript file source targetFramework
