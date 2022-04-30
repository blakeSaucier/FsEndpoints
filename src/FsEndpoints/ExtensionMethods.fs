[<AutoOpen>]
module FsEndpoints.Extensions

open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices
open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open FsEndpoints.Endpoint


type WebApplication with
    member app.UseFsEndpoints (api: FsEndpointDef list) =
        app.UseOpenApi() |> ignore
        app.UseSwaggerUi3(fun config -> config.DocumentPath <- "/swagger.json") |> ignore
        addEndpoints app api |> ignore
        
        // Generate the OpenApi spec
        let openApiSpec =
            api
            |> OpenApi.Generate.generateOpenApiDocument
            |> OpenApi.Write.writeSpec
        
        // Serve the swagger doc
        app.Map("/swagger.json", (fun (ctx:HttpContext) ->
            ctx.Response.ContentType <- "application/json"
            ctx.Response.WriteAsync(openApiSpec)
        )) |> ignore
        app

        
        



