// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Buildalyzer

open Ionide.ProjInfo
open Ionide.ProjInfo.ProjectLoader
open Ionide.ProjInfo.Types
open System
open System.IO

type ProjectType =
    | FSharp
    | CSharp
    | Other

type System.Collections.Generic.IReadOnlyDictionary<'key, 'value> with
    member x.TryFind(k: 'key) =
        match x.TryGetValue(k) with
        | true, v -> Some v
        | false, _ -> None

let toProjInfo customPropertyNames (r: IAnalyzerResult) =
    let projectDir = Path.GetDirectoryName r.ProjectFilePath

    let p2pRefs =
        r.Items.TryFind "_MSBuildProjectReferenceExistent"
        |> Option.map
            (fun items ->
                items
                |> Array.map
                    (fun item ->
                        let relPath = item.ItemSpec
                        let path = Path.GetFullPath(Path.Combine(Path.GetDirectoryName r.ProjectFilePath, relPath))

                        let tfms =
                            item.Metadata.TryFind "TargetFramework"
                            |> Option.orElseWith (fun _ -> item.Metadata.TryFind "TargetFrameworks")
                            |> Option.defaultValue ""

                        { RelativePath = relPath
                          ProjectFileName = path
                          TargetFramework = tfms }: ProjectReference))
        |> Option.defaultValue [||]

    let nugetRefs =
        r.Items.TryFind "Reference"
        |> Option.map
            (fun refs ->
                refs
                |> Array.choose
                    (fun r ->
                        match r.Metadata.TryFind "NuGetSourceType" with
                        | Some "Package" ->
                            match r.Metadata.TryFind "NuGetPackageId", r.Metadata.TryFind "NuGetPackageVersion", r.Metadata.TryFind "HintPath" with
                            | Some name, Some version, Some fullPath ->
                                Some
                                    { Name = name
                                      Version = version
                                      FullPath = fullPath }
                            | _ -> None
                        | _ -> None))
        |> Option.defaultValue [||]

    let projectType =
        match Path.GetExtension r.ProjectFilePath with
        | ".fsproj" -> FSharp
        | ".csproj" -> CSharp
        | _ -> Other

    let compilerArgs =
        match projectType with
        | FSharp ->
            r.Items.TryFind "FscCommandLineArgs"
            |> Option.map (Array.map (fun item -> FscArguments.useFullPaths projectDir item.ItemSpec))
            |> Option.defaultValue [||]
        | CSharp ->
            r.Items.TryFind "CscCommandLineArgs"
            |> Option.map (Array.map (fun item -> CscArguments.useFullPaths projectDir item.ItemSpec))
            |> Option.defaultValue [||]
        | Other -> [||]

    let partitioner =
        match projectType with
        | FSharp -> FscArguments.isSourceFile r.ProjectFilePath
        | CSharp -> CscArguments.isSourceFile r.ProjectFilePath
        | Other -> (fun _ -> false)

    let outputTypeFinder =
        match projectType with
        | FSharp -> FscArguments.outType
        | CSharp -> CscArguments.outType
        | Other -> (fun _ -> ProjectOutputType.Exe)

    let sourceFiles, otherOptions = compilerArgs |> List.ofArray |> List.partition partitioner
    let outputType = outputTypeFinder otherOptions

    let compileItems =

        r.Items.TryFind "Compile"
        |> Option.map
            (fun compiles ->
                compiles
                |> Array.map
                    (fun c ->
                        let name = c.ItemSpec

                        let link = c.Metadata.TryFind "Link"
                        let fullPath = Path.GetFullPath(Path.Combine(projectDir, name))

                        { Name = name
                          Link = link
                          FullPath = fullPath }))
        |> Option.defaultValue [||]
        |> List.ofArray

    let items = sourceFiles |> List.map (VisualTree.getCompileProjectItem compileItems r.ProjectFilePath)

    let pBool (s: string) =
        match s with
        | "" -> None
        | ConditionEquals "True" -> Some true
        | ConditionEquals "False" -> Some false
        | _ -> None

    let pStringList (s: string) =
        match s with
        | "" -> None
        | StringList list -> Some list
        | _ -> None


    let trim (s: string) = s.Trim()
    let item (prop: string) = r.Items.TryFind prop
    let prop (prop: string) = r.Properties.TryFind prop
    let msbuildBool = prop >> Option.map trim >> Option.bind pBool
    let msbuildString = prop >> Option.map trim
    let msbuildStringList = prop >> Option.map trim >> Option.bind pStringList

    let sdkInfo : ProjectSdkInfo =
        let testProj = "IsTestProject" |> msbuildBool |> Option.defaultValue false
        let configuration = "Configuration" |> msbuildString |> Option.defaultValue ""
        let packable = "IsPackable" |> msbuildBool |> Option.defaultValue false
        let tf = "TargetFramework" |> msbuildString |> Option.defaultValue ""
        let tfi = "TargetFrameworkIdentifier" |> msbuildString |> Option.defaultValue ""
        let tfv = "TargetFrameworkVersion" |> msbuildString |> Option.defaultValue ""
        let allProjects = "MSBuildAllProjects" |> msbuildStringList |> Option.defaultValue []
        let toolsVersion = "MSBuildToolsVersion" |> msbuildString |> Option.defaultValue ""
        let assetsFile = "ProjectAssetsFile" |> msbuildString |> Option.defaultValue ""
        let restoreSuccess = "RestoreSuccess" |> msbuildBool |> Option.defaultValue false
        let configurations = "Configurations" |> msbuildStringList |> Option.defaultValue []
        let tfs = "TargetFrameworks" |> msbuildStringList |> Option.defaultValue []
        let runArgs = "RunArguments" |> msbuildString
        let runCommand = "RunCommand" |> msbuildString
        let publishable = "IsPublishable" |> msbuildBool

        { IsTestProject = testProj
          Configuration = configuration
          IsPackable = packable
          TargetFramework = tf
          TargetFrameworkIdentifier = tfi
          TargetFrameworkVersion = tfv
          MSBuildAllProjects = allProjects
          MSBuildToolsVersion = toolsVersion
          ProjectAssetsFile = assetsFile
          RestoreSuccess = restoreSuccess
          Configurations = configurations
          TargetFrameworks = tfs
          RunArguments = runArgs
          RunCommand = runCommand
          IsPublishable = publishable }

    let customProps =
        customPropertyNames
        |> List.choose
            (fun propertyName ->
                prop propertyName
                |> Option.map
                    (fun propItem ->
                        { Name = propertyName
                          Value = propItem }))

    let targetPath = prop "TargetPath" |> Option.map trim |> Option.defaultValue ""

    { CustomProperties = customProps
      ProjectId = Some r.ProjectFilePath
      ProjectFileName = r.ProjectFilePath
      TargetFramework = r.TargetFramework
      SourceFiles = sourceFiles
      OtherOptions = otherOptions
      ReferencedProjects = p2pRefs |> Array.toList
      PackageReferences = nugetRefs |> Array.toList
      LoadTime = DateTime.Now
      TargetPath = targetPath
      ProjectOutputType = outputType
      ProjectSdkInfo = sdkInfo
      Items = items }: ProjectOptions

type BuildalyzerWorkspaceLoader(toolsPath: ToolsPath, graphBuild: bool, globalProperties: Map<string, string> option) =
    let (ToolsPath msbuildDllPath) = toolsPath
    let dotnetExePath = Ionide.ProjInfo.Paths.dotnetRoot

    let globalProperties =
        defaultArg globalProperties Map.empty
        |> Map.toList
        |> List.append [ "BuildingOutOfProcess", "false"
                         "BuildingInsideVisualStudio", "true" ]

    let loadingNotification = new Event<Types.WorkspaceProjectState>()

    let buildArguments =
        if graphBuild then
            [ "--graph" ]
        else
            []

    let buildProjects (manager: AnalyzerManager) customPropertiesToRead makeBinLog =
        // tell subscribers that we're starting
        manager.Projects
        |> Seq.distinctBy (fun (KeyValue (path, _)) -> path)
        |> Seq.map (fun (KeyValue (path, _)) -> path)
        |> Seq.iter (WorkspaceProjectState.Loading >> loadingNotification.Trigger)

        // crack the projects
        let results =
            manager.Projects
            |> Seq.map (fun (KeyValue (_, project)) -> project)
            |> Seq.toArray
            |> Array.Parallel.map
                (fun project ->
                    let tfm = project.ProjectFile.TargetFrameworks.[0]

                    if makeBinLog then
                        project.AddBinaryLogger(System.IO.Path.Combine(System.Environment.CurrentDirectory, System.IO.Path.ChangeExtension(project.ProjectFile.Name, ".binlog")))

                    let projectGlobalProps = ProjectLoader.getGlobalProps project.ProjectFile.Path (Some tfm) msbuildDllPath globalProperties

                    let buildEnvironment =
                        Buildalyzer.Environment.BuildEnvironment(
                            true,
                            true,
                            ProjectLoader.buildArgs,
                            msbuildDllPath,
                            dotnetExePath,
                            buildArguments,
                            additionalGlobalProperties = projectGlobalProps,
                            additionalEnvironmentVariables = dict [ "SkipCompilerExecution", "true" ]
                        )

                    let results = project.Build(buildEnvironment)

                    match results.TryGetTargetFramework(tfm) with
                    | true, tfmResults ->
                        if tfmResults.Succeeded then
                            Ok tfmResults
                        else
                            loadingNotification.Trigger(WorkspaceProjectState.Failed(project.ProjectFile.Path, GetProjectOptionsErrors.GenericError(project.ProjectFile.Path, "Build failed")))
                            Result.Error "Build failed"
                    | false, _ ->
                        loadingNotification.Trigger(WorkspaceProjectState.Failed(project.ProjectFile.Path, GetProjectOptionsErrors.GenericError(project.ProjectFile.Path, "No build results for TFM")))
                        Result.Error "No Build results for TFM")

        let resultsMap =
            results
            |> Array.choose
                (function
                | Ok x -> Some x
                | _ -> None)
            |> List.ofArray

        // map the results
        let successes = resultsMap |> List.map (fun results -> toProjInfo customPropertiesToRead results)

        successes |> List.iter (fun proj -> WorkspaceProjectState.Loaded(proj, successes, false) |> loadingNotification.Trigger)

        successes |> List.toSeq

    interface IWorkspaceLoader with
        member x.LoadSln(path) =
            use logger = new System.IO.StringWriter()
            let manager = AnalyzerManager(path, AnalyzerManagerOptions(LogWriter = logger))
            buildProjects manager [] false

        member x.LoadProjects(projectPaths, customPropertiesToRead, makeBinLog) =
            use logger = new System.IO.StringWriter()
            let manager = AnalyzerManager(AnalyzerManagerOptions(LogWriter = logger))
            // init the manager with all the projects
            projectPaths |> List.iter (manager.GetProject >> ignore<IProjectAnalyzer>)
            buildProjects manager customPropertiesToRead makeBinLog

        member x.LoadProjects(projectPaths) =
            (x :> IWorkspaceLoader)
                .LoadProjects(projectPaths, [], false)

        [<CLIEvent>]
        override x.Notifications = loadingNotification.Publish

    static member Create(toolsPath, ?graphBuild, ?globalProperties) =
        let graphBuild = defaultArg graphBuild false
        let globalProperties = globalProperties |> Option.map Map.ofList
        BuildalyzerWorkspaceLoader(toolsPath, graphBuild, globalProperties) :> IWorkspaceLoader

open System
open Argu

type Args =
    | Version
    | Project of path: string
    | Solution of path: string
    | InProcess
    | OutOfProcess
    | Graph
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Version -> "Display the version of the application"
            | Project (path) -> "Analyze a single project at {path}"
            | Solution (path) -> "Analyze a solution of projects at {path}"
            | Graph -> "Use the graph loader"
            | InProcess -> "Host MSBuild in this process"
            | OutOfProcess -> "Use MSBuild from the host machine"

let parser = Argu.ArgumentParser.Create("proj", "analyze msbuild projects", errorHandler = ProcessExiter(), checkStructure = true)

type LoaderFunc = ToolsPath * list<string * string> -> IWorkspaceLoader

let parseProject (loaderFunc: LoaderFunc) (path: string) =
    let cwd = System.IO.Path.GetDirectoryName path
    let toolsPath = Ionide.ProjInfo.Init.init cwd
    let loader = loaderFunc (toolsPath, [])
    loader.LoadProjects([ path ], [], true)

let parseSolution (loaderFunc: LoaderFunc) (path: string) =
    let cwd = System.IO.Path.GetDirectoryName path
    let toolsPath = Ionide.ProjInfo.Init.init cwd
    let loader = loaderFunc (toolsPath, [])
    loader.LoadSln path

[<EntryPoint>]
let main argv =
    let args = parser.ParseCommandLine(argv, raiseOnUsage = false)

    if args.TryGetResult Version <> None then
        printfn
            $"Ionide.ProjInfo.Tool, v%A{System
                                            .Reflection
                                            .Assembly
                                            .GetExecutingAssembly()
                                            .GetName()
                                            .Version}"

        0
    else

        let loaderFunc : LoaderFunc =
            match args.TryGetResult InProcess, args.TryGetResult OutOfProcess, args.TryGetResult Graph with
            | _, None, Some _ -> fun (p, opts) -> WorkspaceLoaderViaProjectGraph.Create(p, opts)
            | _, None, None -> fun (p, opts) -> WorkspaceLoader.Create(p, opts)
            | _, Some _, Some _ -> fun (p, opts) -> BuildalyzerWorkspaceLoader.Create(p, true, opts)
            | _, Some _, None -> fun (p, opts) -> BuildalyzerWorkspaceLoader.Create(p, false, opts)

        let projects =
            match args.TryGetResult Project with
            | Some path -> parseProject loaderFunc path
            | None ->
                match args.TryGetResult Solution with
                | Some path -> parseSolution loaderFunc path
                | None -> Seq.empty

        let projects = projects |> List.ofSeq

        match projects with
        | [] ->
            failwith "Couldn't parse any projects"
            exit 1
        | projects ->
            printfn "%A" projects
            exit 0
