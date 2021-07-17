namespace Ionide.ProjInfo

open System
open System.Collections.Generic
open Microsoft.Build.Evaluation
open Microsoft.Build.Framework
open System.Runtime.Loader
open System.IO
open Microsoft.Build.Execution
open Types
open Microsoft.Build.Graph
open System.Diagnostics

/// functions for .net sdk probing
module internal SdkDiscovery =

    let msbuildForSdk (sdkPath: string) = Path.Combine(sdkPath, "MSBuild.dll")

    let private versionedPaths (root: string) =
        System.IO.Directory.EnumerateDirectories root
        |> Seq.map (Path.GetFileName >> SemanticVersioning.Version)
        |> Seq.sortDescending
        |> Seq.map (fun v -> v, Path.Combine(root, string v))

    let sdks (dotnetRoot: string) =
        let basePath = Path.GetDirectoryName dotnetRoot
        let sdksPath = Path.Combine(basePath, "sdk")
        versionedPaths sdksPath

    let runtimes (dotnetRoot: string) =
        let basePath = Path.GetDirectoryName dotnetRoot
        let netcoreAppPath = Path.Combine(basePath, "shared", "Microsoft.NETCore.App")
        versionedPaths netcoreAppPath

    /// performs a `dotnet --version` command at the given directory to get the version of the
    /// SDK active at that location.
    let versionAt (cwd: string) =
        let exe = Paths.dotnetRoot
        let info = ProcessStartInfo()
        info.WorkingDirectory <- cwd
        info.FileName <- exe
        info.ArgumentList.Add("--version")
        info.RedirectStandardOutput <- true
        let p = System.Diagnostics.Process.Start(info)
        p.WaitForExit()
        p.StandardOutput.ReadToEnd() |> SemanticVersioning.Version

[<RequireQualifiedAccess>]
module Init =

    let private ensureTrailer (path: string) =
        if path.EndsWith(Path.DirectorySeparatorChar) || path.EndsWith(Path.AltDirectorySeparatorChar) then
            path
        else
            path + string Path.DirectorySeparatorChar

    let mutable private resolveHandler : Func<AssemblyLoadContext, System.Reflection.AssemblyName, System.Reflection.Assembly> = null

    let private resolveFromSdkRoot (root: string) : Func<AssemblyLoadContext, System.Reflection.AssemblyName, System.Reflection.Assembly> =
        Func<AssemblyLoadContext, System.Reflection.AssemblyName, System.Reflection.Assembly>
            (fun assemblyLoadContext assemblyName ->
                let paths = [ Path.Combine(root, assemblyName.Name + ".dll") ]

                match paths |> List.tryFind File.Exists with
                | Some path -> assemblyLoadContext.LoadFromAssemblyPath path
                | None -> null)


    /// <summary>
    /// Given the versioned path to an SDK root, sets up a few required environment variables
    /// that make the SDK and MSBuild behave the same as they do when invoked from the dotnet cli directly.
    /// </summary>
    /// <remarks>
    /// Specifically, this sets the following environment variables:
    /// <list type="bullet">
    /// <item><term>MSBUILD_EXE_PATH</term><description>the path to MSBuild.dll in the SDK</description></item>
    /// <item><term>MSBuildExtensionsPath</term><description>the slash-terminated root path for this SDK version</description></item>
    /// <item><term>MSBuildSDKsPath</term><description>the path to the Sdks folder inside this SDK version</description></item>
    /// <item><term>DOTNET_HOST_PATH</term><description>the path to the 'dotnet' binary</description></item>
    /// </list>
    ///
    /// It also hooks up assembly resolution to execute from the sdk's base path, per the suggestions in the MSBuildLocator project.
    ///
    /// See <see href="https://github.com/microsoft/MSBuildLocator/blob/d83904bff187ce8245f430b93e8b5fbfefb6beef/src/MSBuildLocator/MSBuildLocator.cs#L289">MSBuildLocator</see>,
    /// the <see href="https://github.com/dotnet/sdk/blob/a30e465a2e2ea4e2550f319a2dc088daaafe5649/src/Cli/dotnet/CommandFactory/CommandResolution/MSBuildProject.cs#L120">dotnet cli</see>, and
    /// the <see href="https://github.com/dotnet/sdk/blob/2c011f2aa7a91a386430233d5797452ca0821ed3/src/Cli/Microsoft.DotNet.Cli.Utils/MSBuildForwardingAppWithoutLogging.cs#L42-L44">dotnet msbuild command</see>
    /// for more examples of this.
    /// </remarks>
    /// <param name="sdkRoot">the versioned root path of a given SDK version, for example '/usr/local/share/dotnet/sdk/5.0.300'</param>
    /// <returns></returns>
    let setupForSdkVersion (sdkRoot: string) =
        let msbuild = SdkDiscovery.msbuildForSdk sdkRoot

        // gotta set some env variables so msbuild interop works, see the locator for details: https://github.com/microsoft/MSBuildLocator/blob/d83904bff187ce8245f430b93e8b5fbfefb6beef/src/MSBuildLocator/MSBuildLocator.cs#L289
        Environment.SetEnvironmentVariable("MSBUILD_EXE_PATH", msbuild)
        Environment.SetEnvironmentVariable("MSBuildExtensionsPath", ensureTrailer sdkRoot)
        Environment.SetEnvironmentVariable("MSBuildSDKsPath", Path.Combine(sdkRoot, "Sdks"))

        match System.Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
        | null
        | "" -> Environment.SetEnvironmentVariable("DOTNET_HOST_PATH", Paths.dotnetRoot)
        | alreadySet -> ()

        if resolveHandler <> null then
            AssemblyLoadContext.Default.remove_Resolving resolveHandler

        resolveHandler <- resolveFromSdkRoot sdkRoot
        AssemblyLoadContext.Default.add_Resolving resolveHandler

    /// Initialize the MsBuild integration. Returns path to MsBuild tool that was detected by Locator. Needs to be called before doing anything else.
    /// Call it again when the working directory changes.
    let init (workingDirectory: string) =
        let dotnetSdkVersionAtPath = SdkDiscovery.versionAt workingDirectory
        let sdkVersion, sdkPath = SdkDiscovery.sdks Paths.dotnetRoot |> Seq.skipWhile (fun (v, path) -> v > dotnetSdkVersionAtPath) |> Seq.head
        let msbuild = SdkDiscovery.msbuildForSdk sdkPath
        setupForSdkVersion sdkPath
        ToolsPath msbuild

/// <summary>
/// Low level APIs for single project loading. Doesn't provide caching, and doesn't follow p2p references.
/// In most cases you want to use an <see cref="Ionide.ProjInfo.IWorkspaceLoader"/> type instead
/// </summary>
module ProjectLoader =

    type LoadedProject = internal LoadedProject of ProjectInstance

    [<RequireQualifiedAccess>]
    type ProjectLoadingStatus =
        private
        | Success of LoadedProject
        | Error of string

    let internal logger (writer: StringWriter) =
        { new ILogger with
            member this.Initialize(eventSource: IEventSource) : unit =
                // eventSource.ErrorRaised.Add(fun t -> writer.WriteLine t.Message) //Only log errors
                eventSource.AnyEventRaised.Add(fun t -> writer.WriteLine t.Message)

            member this.Parameters : string = ""

            member this.Parameters
                with set (v: string): unit = printfn "v"

            member this.Shutdown() : unit = ()
            member this.Verbosity : LoggerVerbosity = LoggerVerbosity.Detailed

            member this.Verbosity
                with set (v: LoggerVerbosity): unit = () }

    let getTfm (path: string) readingProps =
        let pi = ProjectInstance(path, globalProperties = readingProps, toolsVersion = null)
        let tfm = pi.GetPropertyValue "TargetFramework"

        if String.IsNullOrWhiteSpace tfm then
            let tfms = pi.GetPropertyValue "TargetFrameworks"

            match tfms with
            | null -> None
            | tfms ->
                match tfms.Split(';') with
                | [||] -> None
                | tfms -> Some tfms.[0]
        else
            Some tfm

    let createLoggers (paths: string seq) (generateBinlog: bool) (sw: StringWriter) =
        let logger = logger (sw)

        if generateBinlog then
            let loggers =
                paths
                |> Seq.map (fun path -> Microsoft.Build.Logging.BinaryLogger(Parameters = Path.Combine(Path.GetDirectoryName(path), "msbuild.binlog")) :> ILogger)

            [ logger; yield! loggers ]
        else
            [ logger ]

    let getGlobalProps (path: string) (tfm: string option) (toolsPath: string) (globalProperties: (string * string) list) =
        dict [ "ProvideCommandLineArgs", "true"
               "DesignTimeBuild", "true"
               "SkipCompilerExecution", "true"
               "GeneratePackageOnBuild", "false"
               "Configuration", "Debug"
               "DefineExplicitDefaults", "true"
               "BuildProjectReferences", "false"
               "UseCommonOutputDirectory", "false"
               if tfm.IsSome then
                   "TargetFramework", tfm.Value
               if path.EndsWith ".csproj" then
                   "NonExistentFile", Path.Combine("__NonExistentSubDir__", "__NonExistentFile__")
               "DotnetProjInfo", "true"
               "MSBuildEnableWorkloadResolver", "false" // we don't care about workloads
               yield! globalProperties ]


    let buildArgs =
        [| "ResolveAssemblyReferencesDesignTime"
           "ResolveProjectReferencesDesignTime"
           "ResolvePackageDependenciesDesignTime"
           "_GenerateCompileDependencyCache"
           "_ComputeNonExistentFileProperty"
           "CoreCompile" |]

    let loadProject (path: string) (generateBinlog: bool) (ToolsPath toolsPath) globalProperties =
        try
            let readingProps = getGlobalProps path None toolsPath globalProperties
            let tfm = getTfm path readingProps

            let globalProperties = getGlobalProps path tfm toolsPath globalProperties

            use pc = new ProjectCollection(globalProperties)

            let pi = pc.LoadProject(path, globalProperties, toolsVersion = null)

            use sw = new StringWriter()

            let loggers = createLoggers [ path ] generateBinlog sw

            let pi = pi.CreateProjectInstance()


            let build = pi.Build(buildArgs, loggers)

            let t = sw.ToString()

            if build then
                ProjectLoadingStatus.Success(LoadedProject pi)
            else
                ProjectLoadingStatus.Error(sw.ToString())
        with
        | exc -> ProjectLoadingStatus.Error(exc.Message)

    let getFscArgs (LoadedProject project) =
        project.Items |> Seq.filter (fun p -> p.ItemType = "FscCommandLineArgs") |> Seq.map (fun p -> p.EvaluatedInclude)

    let getCscArgs (LoadedProject project) =
        project.Items |> Seq.filter (fun p -> p.ItemType = "CscCommandLineArgs") |> Seq.map (fun p -> p.EvaluatedInclude)

    let getP2Prefs (LoadedProject project) =
        project.Items
        |> Seq.filter (fun p -> p.ItemType = "_MSBuildProjectReferenceExistent")
        |> Seq.map
            (fun p ->
                let relativePath = p.EvaluatedInclude
                let path = p.GetMetadataValue "FullPath"

                let tfms =
                    if p.HasMetadata "TargetFramework" then
                        p.GetMetadataValue "TargetFramework"
                    else
                        p.GetMetadataValue "TargetFrameworks"

                { RelativePath = relativePath
                  ProjectFileName = path
                  TargetFramework = tfms })

    let getCompileItems (LoadedProject project) =
        project.Items
        |> Seq.filter (fun p -> p.ItemType = "Compile")
        |> Seq.map
            (fun p ->
                let name = p.EvaluatedInclude

                let link =
                    if p.HasMetadata "Link" then
                        Some(p.GetMetadataValue "Link")
                    else
                        None

                let fullPath = p.GetMetadataValue "FullPath"

                { Name = name
                  FullPath = fullPath
                  Link = link })

    let getNuGetReferences (LoadedProject project) =
        project.Items
        |> Seq.filter (fun p -> p.ItemType = "Reference" && p.GetMetadataValue "NuGetSourceType" = "Package")
        |> Seq.map
            (fun p ->
                let name = p.GetMetadataValue "NuGetPackageId"
                let version = p.GetMetadataValue "NuGetPackageVersion"
                let fullPath = p.GetMetadataValue "FullPath"

                { Name = name
                  Version = version
                  FullPath = fullPath })

    let getProperties (LoadedProject project) (properties: string list) =
        project.Properties
        |> Seq.filter (fun p -> List.contains p.Name properties)
        |> Seq.map
            (fun p ->
                { Name = p.Name
                  Value = p.EvaluatedValue })

    let (|ConditionEquals|_|) (str: string) (arg: string) =
        if System.String.Compare(str, arg, System.StringComparison.OrdinalIgnoreCase) = 0 then
            Some()
        else
            None

    let (|StringList|_|) (str: string) =
        str.Split([| ';' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> Some

    let getSdkInfo (props: Property seq) =

        let msbuildPropBool (s: Property) =
            match s.Value.Trim() with
            | "" -> None
            | ConditionEquals "True" -> Some true
            | _ -> Some false

        let msbuildPropStringList (s: Property) =
            match s.Value.Trim() with
            | "" -> []
            | StringList list -> list
            | _ -> []

        let msbuildPropBool (prop) =
            props |> Seq.tryFind (fun n -> n.Name = prop) |> Option.bind msbuildPropBool

        let msbuildPropStringList prop =
            props |> Seq.tryFind (fun n -> n.Name = prop) |> Option.map msbuildPropStringList

        let msbuildPropString prop =
            props |> Seq.tryFind (fun n -> n.Name = prop) |> Option.map (fun n -> n.Value.Trim())

        { IsTestProject = msbuildPropBool "IsTestProject" |> Option.defaultValue false
          Configuration = msbuildPropString "Configuration" |> Option.defaultValue ""
          IsPackable = msbuildPropBool "IsPackable" |> Option.defaultValue false
          TargetFramework = msbuildPropString "TargetFramework" |> Option.defaultValue ""
          TargetFrameworkIdentifier = msbuildPropString "TargetFrameworkIdentifier" |> Option.defaultValue ""
          TargetFrameworkVersion = msbuildPropString "TargetFrameworkVersion" |> Option.defaultValue ""

          MSBuildAllProjects = msbuildPropStringList "MSBuildAllProjects" |> Option.defaultValue []
          MSBuildToolsVersion = msbuildPropString "MSBuildToolsVersion" |> Option.defaultValue ""

          ProjectAssetsFile = msbuildPropString "ProjectAssetsFile" |> Option.defaultValue ""
          RestoreSuccess = msbuildPropBool "RestoreSuccess" |> Option.defaultValue false

          Configurations = msbuildPropStringList "Configurations" |> Option.defaultValue []
          TargetFrameworks = msbuildPropStringList "TargetFrameworks" |> Option.defaultValue []

          RunArguments = msbuildPropString "RunArguments"
          RunCommand = msbuildPropString "RunCommand"

          IsPublishable = msbuildPropBool "IsPublishable" }

    let mapToProject (path: string) (compilerArgs: string seq) (p2p: ProjectReference seq) (compile: CompileItem seq) (nugetRefs: PackageReference seq) (sdkInfo: ProjectSdkInfo) (props: Property seq) (customProps: Property seq) =
        let projDir = Path.GetDirectoryName path

        let outputType, sourceFiles, otherOptions =
            if path.EndsWith ".fsproj" then
                let fscArgsNormalized =
                    //workaround, arguments in rsp can use relative paths
                    compilerArgs |> Seq.map (FscArguments.useFullPaths projDir) |> Seq.toList

                let sourceFiles, otherOptions = fscArgsNormalized |> List.partition (FscArguments.isSourceFile path)
                let outputType = FscArguments.outType fscArgsNormalized
                outputType, sourceFiles, otherOptions
            else
                let cscArgsNormalized =
                    //workaround, arguments in rsp can use relative paths
                    compilerArgs |> Seq.map (CscArguments.useFullPaths projDir) |> Seq.toList

                let sourceFiles, otherOptions = cscArgsNormalized |> List.partition (CscArguments.isSourceFile path)
                let outputType = CscArguments.outType cscArgsNormalized
                outputType, sourceFiles, otherOptions

        let compileItems = sourceFiles |> List.map (VisualTree.getCompileProjectItem (compile |> Seq.toList) path)

        let project =
            { ProjectId = Some path
              ProjectFileName = path
              TargetFramework = sdkInfo.TargetFramework
              SourceFiles = sourceFiles
              OtherOptions = otherOptions
              ReferencedProjects = List.ofSeq p2p
              PackageReferences = List.ofSeq nugetRefs
              LoadTime = DateTime.Now
              TargetPath = props |> Seq.tryFind (fun n -> n.Name = "TargetPath") |> Option.map (fun n -> n.Value) |> Option.defaultValue ""
              ProjectOutputType = outputType
              ProjectSdkInfo = sdkInfo
              Items = compileItems
              CustomProperties = List.ofSeq customProps }


        project


    let getLoadedProjectInfo (path: string) customProperties project =
        // let (LoadedProject p) = project
        // let path = p.FullPath

        let properties =
            [ "OutputType"
              "IsTestProject"
              "TargetPath"
              "Configuration"
              "IsPackable"
              "TargetFramework"
              "TargetFrameworkIdentifier"
              "TargetFrameworkVersion"
              "MSBuildAllProjects"
              "ProjectAssetsFile"
              "RestoreSuccess"
              "Configurations"
              "TargetFrameworks"
              "RunArguments"
              "RunCommand"
              "IsPublishable"
              "BaseIntermediateOutputPath"
              "TargetPath"
              "IsCrossTargetingBuild"
              "TargetFrameworks" ]

        let p2pRefs = getP2Prefs project

        let comandlineArgs =
            if path.EndsWith ".fsproj" then
                getFscArgs project
            else
                getCscArgs project

        let compileItems = getCompileItems project
        let nuGetRefs = getNuGetReferences project
        let props = getProperties project properties
        let sdkInfo = getSdkInfo props
        let customProps = getProperties project customProperties

        if not sdkInfo.RestoreSuccess then
            Result.Error "not restored"
        else

            let proj = mapToProject path comandlineArgs p2pRefs compileItems nuGetRefs sdkInfo props customProps

            Result.Ok proj

    /// <summary>
    /// Main entry point for project loading.
    /// </summary>
    /// <param name="path">Full path to the `.fsproj` file</param>
    /// <param name="toolsPath">Path to MsBuild obtained from `ProjectLoader.init cwd`</param>
    /// <param name="generateBinlog">Enable Binary Log generation</param>
    /// <param name="globalProperties">The global properties to use (e.g. Configuration=Release). Some additional global properties are pre-set by the tool</param>
    /// <param name="customProperties">List of additional MsBuild properties that you want to obtain.</param>
    /// <returns>Returns the record instance representing the loaded project or string containing error message</returns>
    let getProjectInfo (path: string) (toolsPath: ToolsPath) (globalProperties: (string * string) list) (generateBinlog: bool) (customProperties: string list) : Result<Types.ProjectOptions, string> =
        let loadedProject = loadProject path generateBinlog toolsPath globalProperties

        match loadedProject with
        | ProjectLoadingStatus.Success project -> getLoadedProjectInfo path customProperties project
        | ProjectLoadingStatus.Error e -> Result.Error e

open Ionide.ProjInfo.Logging

module WorkspaceLoaderViaProjectGraph =
    let locker = obj ()


/// A type that turns project files or solution files into deconstructed options.
/// Use this in conjunction with the other ProjInfo libraries to turn these options into
/// ones compatible for use with FCS directly.
type IWorkspaceLoader =

    /// <summary>
    /// Load a list of projects, extracting a set of custom build properties from the build results
    /// in addition to the properties used to power the ProjectOption creation.
    /// </summary>
    /// <returns></returns>
    abstract member LoadProjects : string list * list<string> * bool -> seq<ProjectOptions>

    /// <summary>
    /// Load a list of projects
    /// </summary>
    /// <returns></returns>
    abstract member LoadProjects : string list -> seq<ProjectOptions>

    /// <summary>
    /// Load every project contained in the solution file
    /// </summary>
    /// <returns></returns>
    abstract member LoadSln : string -> seq<ProjectOptions>

    [<CLIEvent>]
    abstract Notifications : IEvent<WorkspaceProjectState>

type WorkspaceLoaderViaProjectGraph private (toolsPath, ?globalProperties: (string * string) list) =
    let (ToolsPath toolsPath) = toolsPath
    let globalProperties = defaultArg globalProperties []
    let logger = LogProvider.getLoggerFor<WorkspaceLoaderViaProjectGraph> ()
    let loadingNotification = new Event<Types.WorkspaceProjectState>()

    let handleProjectGraphFailures f =
        try
            f () |> Some
        with
        | :? Microsoft.Build.Exceptions.InvalidProjectFileException as e ->
            let p = e.ProjectFile
            loadingNotification.Trigger(WorkspaceProjectState.Failed(p, ProjectNotFound(p)))
            None
        | ex ->
            logger.error (Log.setMessage "oops" >> Log.addExn ex)
            None

    let projectInstanceFactory projectPath (_globalProperties: IDictionary<string, string>) (projectCollection: ProjectCollection) =
        let tfm = ProjectLoader.getTfm projectPath (dict globalProperties)
        //let globalProperties = globalProperties |> Seq.toList |> List.map (fun (KeyValue(k,v)) -> (k,v))
        let globalProperties = ProjectLoader.getGlobalProps projectPath tfm toolsPath globalProperties
        ProjectInstance(projectPath, globalProperties, toolsVersion = null, projectCollection = projectCollection)

    let projectGraphProjs (paths: string seq) =

        handleProjectGraphFailures
        <| fun () ->
            paths |> Seq.iter (fun p -> loadingNotification.Trigger(WorkspaceProjectState.Loading p))

            let graph =
                match paths |> List.ofSeq with
                | [ x ] -> ProjectGraph(x, projectCollection = ProjectCollection.GlobalProjectCollection, projectInstanceFactory = projectInstanceFactory)
                | xs ->
                    let entryPoints = paths |> Seq.map ProjectGraphEntryPoint |> List.ofSeq
                    ProjectGraph(entryPoints, projectCollection = ProjectCollection.GlobalProjectCollection, projectInstanceFactory = projectInstanceFactory)

            graph

    let projectGraphSln (path: string) =
        handleProjectGraphFailures
        <| fun () ->
            let pg = ProjectGraph(path, ProjectCollection.GlobalProjectCollection, projectInstanceFactory)

            pg.ProjectNodesTopologicallySorted
            |> Seq.distinctBy (fun p -> p.ProjectInstance.FullPath)
            |> Seq.map (fun p -> p.ProjectInstance.FullPath)
            |> Seq.iter (fun p -> loadingNotification.Trigger(WorkspaceProjectState.Loading p))

            pg

    let loadProjects (projects: ProjectGraph, customProperties: string list, generateBinlog: bool) =
        try
            lock WorkspaceLoaderViaProjectGraph.locker
            <| fun () ->
                let allKnown = projects.ProjectNodesTopologicallySorted |> Seq.distinctBy (fun p -> p.ProjectInstance.FullPath)

                let allKnownNames = allKnown |> Seq.map (fun p -> p.ProjectInstance.FullPath) |> Seq.toList

                logger.info (
                    Log.setMessage "Started loading projects {count} {projects}"
                    >> Log.addContextDestructured "count" (allKnownNames |> Seq.length)
                    >> Log.addContextDestructured "projects" (allKnownNames)
                )

                let gbr = GraphBuildRequestData(projects, ProjectLoader.buildArgs, null, BuildRequestDataFlags.ReplaceExistingProjectInstance)
                let bm = BuildManager.DefaultBuildManager
                use sw = new StringWriter()
                let loggers = ProjectLoader.createLoggers allKnownNames generateBinlog sw
                let buildParameters = BuildParameters(Loggers = loggers)
                buildParameters.ProjectLoadSettings <- ProjectLoadSettings.RecordEvaluatedItemElements ||| ProjectLoadSettings.ProfileEvaluation
                buildParameters.LogInitialPropertiesAndItems <- true
                bm.BeginBuild(buildParameters)

                let result = bm.BuildRequest gbr

                bm.EndBuild()

                let rec walkResults (known: String Set) (results: ProjectGraphNode seq) =
                    seq {
                        for nodeKey in results do
                            if known |> Set.contains nodeKey.ProjectInstance.FullPath |> not then
                                yield nodeKey
                                let cache = Set.add nodeKey.ProjectInstance.FullPath known
                                yield! walkResults cache nodeKey.ProjectReferences
                            else
                                yield! walkResults known nodeKey.ProjectReferences
                    }

                let resultsByNode = walkResults Set.empty (result.ResultsByNode |> Seq.map (fun kvp -> kvp.Key)) |> Seq.cache
                let buildProjs = resultsByNode |> Seq.map (fun p -> p.ProjectInstance.FullPath) |> Seq.toList

                logger.info (
                    Log.setMessage "{overallCode}, projects built {count} {projects} "
                    >> Log.addContextDestructured "count" (buildProjs |> Seq.length)
                    >> Log.addContextDestructured "projects" (buildProjs)
                    >> Log.addContextDestructured "overallCode" result.OverallResult
                    >> Log.addExn result.Exception
                )

                let projects =
                    resultsByNode
                    |> Seq.map
                        (fun p ->

                            p.ProjectInstance.FullPath, ProjectLoader.getLoadedProjectInfo p.ProjectInstance.FullPath customProperties (ProjectLoader.LoadedProject p.ProjectInstance))

                    |> Seq.choose
                        (fun (projectPath, projectOptionResult) ->
                            match projectOptionResult with
                            | Ok projectOptions ->

                                Some projectOptions
                            | Error e ->
                                logger.error (Log.setMessage "Failed loading projects {error}" >> Log.addContextDestructured "error" e)
                                loadingNotification.Trigger(WorkspaceProjectState.Failed(projectPath, GenericError(projectPath, e)))
                                None)

                let allProjectOptions = projects |> Seq.toList

                allProjectOptions
                |> Seq.iter
                    (fun po ->
                        logger.info (Log.setMessage "Project loaded {project}" >> Log.addContextDestructured "project" po.ProjectFileName)
                        loadingNotification.Trigger(WorkspaceProjectState.Loaded(po, allProjectOptions |> Seq.toList, false)))

                allProjectOptions :> seq<_>
        with
        | e ->
            let msg = e.Message

            logger.error (Log.setMessage "Failed loading" >> Log.addExn e)

            projects.ProjectNodesTopologicallySorted
            |> Seq.distinctBy (fun p -> p.ProjectInstance.FullPath)
            |> Seq.iter
                (fun p ->

                    let p = p.ProjectInstance.FullPath

                    if msg.Contains "The project file could not be loaded." then
                        loadingNotification.Trigger(WorkspaceProjectState.Failed(p, ProjectNotFound(p)))
                    elif msg.Contains "not restored" then
                        loadingNotification.Trigger(WorkspaceProjectState.Failed(p, ProjectNotRestored(p)))
                    else
                        loadingNotification.Trigger(WorkspaceProjectState.Failed(p, GenericError(p, msg))))

            Seq.empty

    interface IWorkspaceLoader with
        override this.LoadProjects(projects: string list, customProperties, generateBinlog: bool) =
            projectGraphProjs projects
            |> Option.map (fun pg -> loadProjects (pg, customProperties, generateBinlog))
            |> Option.defaultValue Seq.empty

        override this.LoadProjects(projects: string list) = this.LoadProjects(projects, [], false)

        override this.LoadSln(sln) = this.LoadSln(sln, [], false)

        [<CLIEvent>]
        override this.Notifications = loadingNotification.Publish

    member this.LoadProjects(projects: string list, customProperties: string list, generateBinlog: bool) =
        (this :> IWorkspaceLoader)
            .LoadProjects(projects, customProperties, generateBinlog)

    member this.LoadProjects(projects: string list, customProperties) =
        this.LoadProjects(projects, customProperties, false)




    member this.LoadProject(project: string, customProperties: string list, generateBinlog: bool) =
        this.LoadProjects([ project ], customProperties, generateBinlog)

    member this.LoadProject(project: string, customProperties: string list) =
        this.LoadProjects([ project ], customProperties)

    member this.LoadProject(project: string) =
        (this :> IWorkspaceLoader)
            .LoadProjects([ project ])


    member this.LoadSln(sln: string, customProperties: string list, generateBinlog: bool) =
        projectGraphSln sln
        |> Option.map (fun pg -> loadProjects (pg, customProperties, generateBinlog))
        |> Option.defaultValue Seq.empty

    member this.LoadSln(sln, customProperties) =
        this.LoadSln(sln, customProperties, false)


    static member Create(toolsPath: ToolsPath, ?globalProperties) =
        WorkspaceLoaderViaProjectGraph(toolsPath, ?globalProperties = globalProperties) :> IWorkspaceLoader

type WorkspaceLoader private (toolsPath: ToolsPath, ?globalProperties: (string * string) list) =
    let globalProperties = defaultArg globalProperties []
    let loadingNotification = new Event<Types.WorkspaceProjectState>()

    interface IWorkspaceLoader with

        [<CLIEvent>]
        override __.Notifications = loadingNotification.Publish

        override __.LoadProjects(projects: string list, customProperties, generateBinlog: bool) =
            let cache = Dictionary<string, ProjectOptions>()

            let getAllKnonw () =
                cache |> Seq.map (fun n -> n.Value) |> Seq.toList

            let rec loadProject p =
                let res = ProjectLoader.getProjectInfo p toolsPath globalProperties generateBinlog customProperties

                match res with
                | Ok project ->
                    try
                        cache.Add(p, project)
                        let lst = project.ReferencedProjects |> Seq.map (fun n -> n.ProjectFileName) |> Seq.toList
                        let info = Some project
                        lst, info
                    with
                    | exc ->
                        loadingNotification.Trigger(WorkspaceProjectState.Failed(p, GenericError(p, exc.Message)))
                        [], None
                | Error msg when msg.Contains "The project file could not be loaded." ->
                    loadingNotification.Trigger(WorkspaceProjectState.Failed(p, ProjectNotFound(p)))
                    [], None
                | Error msg when msg.Contains "not restored" ->
                    loadingNotification.Trigger(WorkspaceProjectState.Failed(p, ProjectNotRestored(p)))
                    [], None
                | Error msg when msg.Contains "The operation cannot be completed because a build is already in progress." ->
                    //Try to load project again
                    Threading.Thread.Sleep(50)
                    loadProject p
                | Error msg ->
                    loadingNotification.Trigger(WorkspaceProjectState.Failed(p, GenericError(p, msg)))
                    [], None

            let rec loadProjectList (projectList: string list) =
                for p in projectList do
                    let newList, toTrigger =
                        if cache.ContainsKey p then
                            let project = cache.[p]
                            loadingNotification.Trigger(WorkspaceProjectState.Loaded(project, getAllKnonw (), true)) //TODO: Should it even notify here?
                            let lst = project.ReferencedProjects |> Seq.map (fun n -> n.ProjectFileName) |> Seq.toList
                            lst, None
                        else
                            loadingNotification.Trigger(WorkspaceProjectState.Loading p)
                            loadProject p


                    loadProjectList newList

                    toTrigger
                    |> Option.iter (fun project -> loadingNotification.Trigger(WorkspaceProjectState.Loaded(project, getAllKnonw (), false)))

            loadProjectList projects
            cache |> Seq.map (fun n -> n.Value)

        override this.LoadProjects(projects) = this.LoadProjects(projects, [], false)

        override this.LoadSln(sln) = this.LoadSln(sln, [], false)

    /// <inheritdoc />
    member this.LoadProjects(projects: string list, customProperties: string list, generateBinlog: bool) =
        (this :> IWorkspaceLoader)
            .LoadProjects(projects, customProperties, generateBinlog)

    member this.LoadProjects(projects, customProperties) =
        this.LoadProjects(projects, customProperties, false)



    member this.LoadProject(project, customProperties: string list, generateBinlog: bool) =
        this.LoadProjects([ project ], customProperties, generateBinlog)

    member this.LoadProject(project, customProperties: string list) =
        this.LoadProjects([ project ], customProperties)

    member this.LoadProject(project) =
        (this :> IWorkspaceLoader)
            .LoadProjects([ project ])


    member this.LoadSln(sln, customProperties: string list, generateBinlog: bool) =
        match InspectSln.tryParseSln sln with
        | Ok (_, slnData) ->
            let projs = InspectSln.loadingBuildOrder slnData
            this.LoadProjects(projs, customProperties, generateBinlog)
        | Error d -> failwithf "Cannot load the sln: %A" d

    member this.LoadSln(sln, customProperties) =
        this.LoadSln(sln, customProperties, false)



    static member Create(toolsPath: ToolsPath, ?globalProperties) =
        WorkspaceLoader(toolsPath, ?globalProperties = globalProperties) :> IWorkspaceLoader

type ProjectViewerTree =
    { Name: string
      Items: ProjectViewerItem list }

and [<RequireQualifiedAccess>] ProjectViewerItem = Compile of string * ProjectViewerItemConfig

and ProjectViewerItemConfig = { Link: string }

module ProjectViewer =

    let render (proj: ProjectOptions) =

        let compileFiles =
            let sources = proj.Items

            //the generated assemblyinfo.fs are not shown as sources
            let isGeneratedAssemblyinfo (name: string) =
                let projName = proj.ProjectFileName |> Path.GetFileNameWithoutExtension
                //TODO check is in `obj` dir for the tfm
                //TODO better, get the name from fsproj
                //TODO cs too
                name.EndsWith(sprintf "%s.AssemblyInfo.fs" projName)

            sources
            |> List.choose
                (function
                | ProjectItem.Compile (name, fullpath) -> Some(name, fullpath))
            |> List.filter (fun (_, p) -> not (isGeneratedAssemblyinfo p))

        { ProjectViewerTree.Name = proj.ProjectFileName |> Path.GetFileNameWithoutExtension
          Items =
              compileFiles
              |> List.map (fun (name, fullpath) -> ProjectViewerItem.Compile(fullpath, { ProjectViewerItemConfig.Link = name })) }
