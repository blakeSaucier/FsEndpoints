[<AutoOpen>]
module FsEndpoints.Endpoint

open System
open Microsoft.AspNetCore.Builder
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Core
open System.Net

type MediaType =
    ApplicationJson | TextHtml
    static member toOpenApi = function
        | ApplicationJson -> "application/json"
        | TextHtml -> "text/html"

type EndpointResponse =
    { Status: HttpStatusCode
      Content: MediaType
      Response: Type option }
    
type EndpointRequest =
    { Content: MediaType
      Request: Type }

type FsEndpointSchema =
    { Responses: EndpointResponse list
      Request: EndpointRequest option }

type FsEndpointMeta =
    { Content: MediaType
      Response: Type option
      Request: Type option }
type HttpVerb = GET | POST | PUT

type FsEndpointDef =
    { Name: string
      Path: string
      Schema: FsEndpointSchema option
      Verb: HttpVerb
      Handler: HttpHandler }
    
let returns<'Res> =
    { Content = ApplicationJson
      Response = Some typeof<'Res>
      Request = None }
    
let meta<'Res,'Req> =
    { Content = ApplicationJson
      Response = Some typeof<'Res>
      Request = Some typeof<'Req> }

let tryGenerateOperationName (path: string) (verb: HttpVerb) =
    let root =
        path.Split("/")
        |> Array.toList
        |> function
        | [ "" ] -> "/"
        | "" :: tail -> tail.Head
        | _ -> "/"
    
    let verbName =
        match verb with
        | GET -> "GET"
        | POST -> "Create"
        | PUT -> "Update"
    
    $"{verbName} {root}"
    
let earlyReturn : HttpFunc = Some >> Task.FromResult
    
let private handleResult (result : HttpContext option) (ctx : HttpContext) =
    match result with
    | None   -> ctx.SetStatusCode (int HttpStatusCode.UnprocessableEntity)
    | Some _ -> ()
    
let toRequestDelegate (handler: HttpHandler) =
    let func = handler earlyReturn
    fun (ctx: HttpContext) ->
        task {
            let! res = func ctx
            return handleResult res ctx
        } :> Task
                                                        
let private addEndpoint (app: WebApplication) (e: FsEndpointDef) =
    app.UseRouting() |> ignore
    app.UseEndpoints(fun c ->
        let asRequestDel =
            toRequestDelegate e.Handler
            |> (fun f -> RequestDelegate(f))
        let verb = e.Verb.ToString()
        c.MapMethods(pattern = e.Path, httpMethods = [verb], requestDelegate = asRequestDel) |> ignore
        ()
        ) |> ignore
    ()
    
let addEndpoints (app: WebApplication) (endpoints: FsEndpointDef list) =
    endpoints |> List.iter (addEndpoint app)
    app