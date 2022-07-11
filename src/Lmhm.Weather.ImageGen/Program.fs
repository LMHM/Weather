module Lmhm.Weather.ImageGen.Program

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Polly   
open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Configuration
open Serilog

type Settings(
    period: int,
    startSecond: int,
    inputPath: string,
    outputPath: string) =

    let mutable period = period
    let mutable startSecond = startSecond
    let mutable inputPath = inputPath
    let mutable outputPath = outputPath

    member this.Period with get() = period and set(value) = period <- value
    member this.StartSecond with get() = startSecond and set(value) = startSecond <- value
    member this.InputPath with get() = inputPath and set(value) = inputPath <- value
    member this.OutputPath with get() = outputPath and set(value) = outputPath <- value
    new() = Settings(Int32.MaxValue, Int32.MaxValue, String.Empty, String.Empty)

type TimedService(
    logger: ILogger,
    name: string,
    settings: Settings,
    func: string -> string -> unit) =

    let timer =
        new Timer(fun obj -> 
            logger.Information("Running " + name)
            try
                try
                    func settings.InputPath settings.OutputPath
                with
                | ex -> logger.Error(ex, "Failed to run " + name)
            finally
                logger.Information("Completed " + name))

    interface IHostedService with
        member this.StartAsync(cancellationToken: CancellationToken) = 
            logger.Information("Starting Service:" + name)
            let due = 60 - DateTime.Now.Second + settings.StartSecond
            timer.Change(due * 1000, settings.Period) |> ignore
            Task.CompletedTask

        member this.StopAsync(cancellationToken: CancellationToken) = 
            logger.Information("Stopping Service:" + name)
            timer.Change(Timeout.Infinite, 0) |> ignore
            Task.CompletedTask

    interface IDisposable with
        member this.Dispose() = timer.Dispose()

type MonthService(logger: ILogger, settings: Settings) = 
    inherit TimedService(logger, "Month", settings, Month.run)

type AverageTempService(logger: ILogger, settings: Settings) =
    inherit TimedService(logger, "AverageTemp", settings, AverageTemp.run)

let monthService (cfg: IConfiguration) (sp: IServiceProvider) =
    new MonthService(
        sp.GetRequiredService<ILogger>(),
        cfg.GetSection("Month").Get<Settings>())

let averageTempService (cfg: IConfiguration) (sp: IServiceProvider) =
    new AverageTempService(
        sp.GetRequiredService<ILogger>(),
        cfg.GetSection("AverageTemp").Get<Settings>())

let addServices (ctx: HostBuilderContext) (svc: IServiceCollection) =
    svc.AddHostedService(monthService ctx.Configuration)
       .AddHostedService(averageTempService ctx.Configuration)
    |> ignore

let serilogConfig (ctx: HostBuilderContext) (cfg: LoggerConfiguration) =
    cfg.ReadFrom.Configuration(ctx.Configuration)
    |> ignore

let app arg =
    Host.CreateDefaultBuilder(arg)
        .ConfigureServices(addServices)
        .UseSerilog(serilogConfig)
        .Build()
        .Run()

[<EntryPoint>]
let main arg =
    Policy
        .Handle<Exception>()
        .WaitAndRetryForever(fun i ctx -> TimeSpan.FromSeconds(10))
        .Execute(fun () -> app arg)
    0
