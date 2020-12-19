namespace Dotnet.ProjInfo.ProjectSystem

open System
open System.IO
open System.Collections.Concurrent
open FSharp.Compiler.SourceCodeServices
open Dotnet.ProjInfo.Types
open Dotnet.ProjInfo
open Workspace

type ProjectResult =
    { projectFileName: string
      projectFiles: List<string>
      outFileOpt: string option
      references: string list
      extra: ProjectOptions
      projectItems: ProjectViewerItem list
      additionals: Map<string, string> }

[<RequireQualifiedAccess>]
type ProjectResponse =
    | Project of project: ProjectResult
    | ProjectError of errorDetails: GetProjectOptionsErrors
    | ProjectLoading of projectFileName: string
    | WorkspaceLoad of finished: bool

/// Public API for any operations related to workspace and projects.
/// Internally keeps all the information related to project files in current workspace.
/// It's responsible for refreshing and caching - should be used only as source of information and public API
type ProjectController(checker: FSharpChecker, toolsPath: ToolsPath) =
    let fileCheckOptions = ConcurrentDictionary<string, FSharpProjectOptions>()
    let projects = ConcurrentDictionary<string, Project>()
    let mutable isWorkspaceReady = false
    let workspaceReady = Event<unit>()
    let notify = Event<ProjectResponse>()

    let updateState (response: ProjectCrackerCache) =
        let normalizeOptions (opts: FSharpProjectOptions) =
            { opts with
                  SourceFiles = opts.SourceFiles |> Array.map (Path.GetFullPath)
                  OtherOptions =
                      opts.OtherOptions
                      |> Array.map
                          (fun n ->
                              if FscArguments.isCompileFile (n)
                              then Path.GetFullPath n
                              else n) }

        for file in response.Items
                    |> List.choose
                        (function
                        | ProjectViewerItem.Compile (p, _) -> Some p) do
            fileCheckOptions.[file] <- normalizeOptions response.Options


    let toProjectCache (opts: FSharpProjectOptions, extraInfo: ProjectOptions, projViewerItems: ProjectViewerItem list) =
        let outFileOpt = Some(extraInfo.TargetPath)
        let references = FscArguments.references (opts.OtherOptions |> List.ofArray)
        let fullPathNormalized = Path.GetFullPath >> Utils.normalizePath

        let projViewerItemsNormalized =
            if obj.ReferenceEquals(null, projViewerItems)
            then []
            else projViewerItems

        let projViewerItemsNormalized =
            projViewerItemsNormalized
            |> List.map
                (function
                | ProjectViewerItem.Compile (p, c) -> ProjectViewerItem.Compile(fullPathNormalized p, c))

        let cached =
            { ProjectCrackerCache.Options = opts
              OutFile = outFileOpt
              References = references
              ExtraInfo = extraInfo
              Items = projViewerItemsNormalized }

        (opts.ProjectFileName, cached)

    member private x.LoaderLoop =
        MailboxProcessor.Start
            (fun agent ->
                let rec loop () =
                    async {
                        let! ((fn, ol, gb), reply: AsyncReplyChannel<_>) = agent.Receive()
                        let mutable wasInvoked = false

                        let! x =
                            Async.FromContinuations
                                (fun (succ, err, cancl) ->
                                    let opl str cache bl =
                                        ol str cache bl

                                        if wasInvoked then
                                            ()
                                        else
                                            wasInvoked <- true
                                            succ ()

                                    x.LoadWorkspace [ fn ] opl gb |> Async.Ignore |> Async.Start)

                        reply.Reply true
                        return ()
                    }

                loop ())

    member __.WorkspaceReady = workspaceReady.Publish

    member __.NotifyWorkspace = notify.Publish

    member __.IsWorkspaceReady = isWorkspaceReady

    member __.GetProjectOptions(file: string): FSharpProjectOptions option =
        let file = Utils.normalizePath file
        fileCheckOptions.TryFind file

    member __.SetProjectOptions(file: string, opts: FSharpProjectOptions) =
        let file = Utils.normalizePath file
        fileCheckOptions.AddOrUpdate(file, (fun _ -> opts), (fun _ _ -> opts)) |> ignore

    member __.RemoveProjectOptions(file) =
        let file = Utils.normalizePath file
        fileCheckOptions.TryRemove file |> ignore

    member __.ProjectOptions = fileCheckOptions |> Seq.map (|KeyValue|)

    member __.GetProject(file: string): Project option =
        let file = Utils.normalizePath file
        projects.TryFind file

    member __.Projects = projects |> Seq.map (|KeyValue|)

    member x.LoadProject projectFileName onProjectLoaded (generateBinlog: bool) =
        x.LoaderLoop.PostAndAsyncReply(fun acr -> (projectFileName, onProjectLoaded, generateBinlog), acr)


    member x.LoadWorkspace (files: string list) onProjectLoaded (generateBinlog: bool) =
        async {
            //TODO check full path
            let projectFileNames = files |> List.map Path.GetFullPath

            let onChange fn =
                x.LoadProject fn onProjectLoaded generateBinlog |> Async.Ignore |> Async.Start

            let prjs = projectFileNames |> List.map (fun projectFileName -> projectFileName, new Project(projectFileName, onChange))

            for projectFileName, proj in prjs do
                projects.[projectFileName] <- proj

            let projectLoadedSuccessfully projectFileName response =
                let project =
                    match projects.TryFind projectFileName with
                    | Some prj -> prj
                    | None ->
                        let proj = new Project(projectFileName, onChange)
                        projects.[projectFileName] <- proj
                        proj

                project.Response <- Some response

                updateState response
                onProjectLoaded projectFileName response

            let onLoaded p =
                match p with
                | ProjectSystemState.Loading projectFileName -> ProjectResponse.ProjectLoading projectFileName |> notify.Trigger
                | ProjectSystemState.Loaded (opts, extraInfo, projectFiles, isFromCache) ->
                    let projectFileName, response = toProjectCache (opts, extraInfo, projectFiles)
                    projectLoadedSuccessfully projectFileName response isFromCache

                    let responseFiles =
                        response.Items
                        |> List.choose
                            (function
                            | ProjectViewerItem.Compile (p, _) -> Some p)

                    let projInfo: ProjectResult =
                        { projectFileName = projectFileName
                          projectFiles = responseFiles
                          outFileOpt = response.OutFile
                          references = response.References
                          extra = response.ExtraInfo
                          projectItems = projectFiles
                          additionals = Map.empty }

                    ProjectResponse.Project projInfo |> notify.Trigger
                | ProjectSystemState.Failed (projectFileName, error) -> ProjectResponse.ProjectError error |> notify.Trigger

            ProjectResponse.WorkspaceLoad false |> notify.Trigger

            // this is to delay the project loading notification (of this thread)
            // after the workspaceload started response returned below in outer async
            // Make test output repeteable, and notification in correct order
            match Environment.workspaceLoadDelay () with
            | delay when delay > TimeSpan.Zero -> do! Async.Sleep(Environment.workspaceLoadDelay().TotalMilliseconds |> int)
            | _ -> ()

            let loader = WorkspaceLoader.Create(toolsPath)

            let bindNewOnloaded (n: WorkspaceProjectState): ProjectSystemState option =
                match n with
                | WorkspaceProjectState.Loading (path) -> Some(ProjectSystemState.Loading path)
                | WorkspaceProjectState.Loaded (opts, allKNownProjects, isFromCache) ->
                    let fcsOpts = FCS.mapToFSharpProjectOptions opts allKNownProjects

                    match Workspace.extractOptionsDPW fcsOpts with
                    | Ok optsDPW ->
                        let view = ProjectViewer.render optsDPW
                        Some(ProjectSystemState.Loaded(fcsOpts, optsDPW, view.Items, isFromCache))
                    | Error _ -> None //TODO not ignore the error
                | WorkspaceProjectState.Failed (path, e) ->
                    let error = e
                    Some(ProjectSystemState.Failed(path, error))

            loader.Notifications.Add(fun arg -> arg |> bindNewOnloaded |> Option.iter onLoaded)

            do! Workspace.loadInBackground onLoaded loader (prjs |> List.map snd) generateBinlog

            ProjectResponse.WorkspaceLoad true |> notify.Trigger

            isWorkspaceReady <- true
            workspaceReady.Trigger()

            return true
        }

    member __.PeekWorkspace (dir: string) (deep: int) (excludedDirs: string list) = WorkspacePeek.peek dir deep excludedDirs
