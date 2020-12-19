// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Dotnet.ProjInfo

open Expecto
open Expecto.Impl
open Expecto.Logging


[<EntryPoint>]
let main argv =
    let toolsPath = Init.init ()
    Tests.runTests {defaultConfig with printer = TestPrinters.summaryPrinter defaultConfig.printer; verbosity = LogLevel.Info } (Tests.tests toolsPath)