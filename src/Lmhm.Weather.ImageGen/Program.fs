module Lmhm.Weather.ImageGen.Program

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Options
open Polly
open Serilog

type Settings(period: int, startSecond: int, inputPath: string, outputPath: string) =

    let mutable period = period
    let mutable startSecond = startSecond
    let mutable inputPath = inputPath
    let mutable outputPath = outputPath

    member this.Period
        with get () = period
        and set (value) = period <- value

    member this.StartSecond
        with get () = startSecond
        and set (value) = startSecond <- value

    member this.InputPath
        with get () = inputPath
        and set (value) = inputPath <- value

    member this.OutputPath
        with get () = outputPath
        and set (value) = outputPath <- value

    new() = Settings(Int32.MaxValue, Int32.MaxValue, String.Empty, String.Empty)

type TimedService(logger: ILogger, options: IOptions<Settings>) =

    let settings = options.Value

    let timer =
        new Timer(fun obj ->
            logger.Information("Running image generation")

            try
                try
                    Day.run settings.InputPath settings.OutputPath
                    Month.run settings.InputPath settings.OutputPath
                    Year.run settings.InputPath settings.OutputPath
                with ex ->
                    logger.Error(ex, "Failed to run image generation")
            finally
                logger.Information("Completed image generation"))

    interface IHostedService with
        member this.StartAsync(cancellationToken: CancellationToken) =
            logger.Information("Starting Timer")
            let due = 60 - DateTime.Now.Second + settings.StartSecond
            timer.Change(due * 1000, settings.Period * 1000) |> ignore
            Task.CompletedTask

        member this.StopAsync(cancellationToken: CancellationToken) =
            logger.Information("Stopping Timer")
            timer.Change(Timeout.Infinite, 0) |> ignore
            Task.CompletedTask

    interface IDisposable with
        member this.Dispose() = timer.Dispose()

let addServices (ctx: HostBuilderContext) (svc: IServiceCollection) =
    svc
        .Configure<Settings>(ctx.Configuration.GetSection("Settings"))
        .AddHostedService<TimedService>()
    |> ignore

let serilogConfig (ctx: HostBuilderContext) (cfg: LoggerConfiguration) =
    cfg.ReadFrom.Configuration(ctx.Configuration) |> ignore

let app arg =
    Host
        .CreateDefaultBuilder(arg)
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
