module FsEndpoints.Middleware

open Endpoint
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Giraffe
open System.Threading.Tasks

type FsEndpointsMiddleware (next          : RequestDelegate,
                            endpoints     : FsEndpointDef list,
                            loggerFactory : ILoggerFactory) =

    let logger = loggerFactory.CreateLogger<FsEndpointsMiddleware>()
    let earlyReturn : HttpFunc = Some >> Task.FromResult

    member __.Invoke (ctx : HttpContext) =
        task {
            let start = System.Diagnostics.Stopwatch.GetTimestamp();
            
            let reqPath = ctx.Request.Path
            let maybeEndpoint = endpoints |> List.tryFind (fun e -> reqPath.Equals(e.Path))
            match maybeEndpoint with
            | Some e ->
                let! _ = e.Handler earlyReturn ctx
                if logger.IsEnabled LogLevel.Debug then
                    let freq = double System.Diagnostics.Stopwatch.Frequency
                    let stop = System.Diagnostics.Stopwatch.GetTimestamp()
                    let elapsedMs = (double (stop - start)) * 1000.0 / freq
                    logger.LogDebug(
                        "Executed Endpoint {Endpoint} {HttpProtocol} {HttpMethod} at {Path} in {ElapsedMs}",
                        e.Name,
                        ctx.Request.Protocol,
                        ctx.Request.Method,
                        ctx.Request.Path.ToString(),
                        elapsedMs)
            | None ->
                return! next.Invoke ctx
        }
