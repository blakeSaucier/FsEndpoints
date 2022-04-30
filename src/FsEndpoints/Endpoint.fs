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

type FsEndpointMeta =
    { OperationName: string
      Produces: MediaType
      Response: Type option
      Request: Type option }
    
let defaultMeta =
    { OperationName = ""
      Produces = ApplicationJson
      Response = None
      Request = None }

type HttpVerb = GET | POST | PUT

type FsEndpointDef =
    { Name: string
      Path: string
      Meta: FsEndpointMeta option
      Verb: HttpVerb
      Handler: HttpHandler }
    
type EndpointMetaBuilder () =
    member _.Yield _ =  defaultMeta
        
    member _.Run (s: FsEndpointMeta) = s
            
    [<CustomOperation("name")>]
    member _.Name (state: FsEndpointMeta, name: string) =
        { state with OperationName = name }
        
    [<CustomOperation("returnType")>]        
    member _.Returns (state: FsEndpointMeta, t: Type) =
        { state with Response = Some t }
        
    [<CustomOperation("accepts")>]
    member _.Accepts (state: FsEndpointMeta, t: Type) =
        { state with Request = Some t }        

let metaBuilder = EndpointMetaBuilder()

let returns<'t> = metaBuilder {
    returnType typeof<'t>
}

let tryGenerateOperationName (path: string) (verb: HttpVerb) =
    let root =
        path.Split("/")
        |> Array.toList
        |> function
        | [ "" ] -> "/"
        | "" :: tail -> tail.Head
        | _ -> "/"
    $"{verb} {root}"
    
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
        
type Endpoint() =
    static member get (path: string, handler: HttpHandler) =
        let guessAName = tryGenerateOperationName path GET
        { Name = guessAName 
          Path = path
          Meta = None
          Verb = GET
          Handler = handler }
        
    static member get<'Returns> (path: string, handler: HttpHandler) =
        let guessAName = tryGenerateOperationName path GET
        { Name = guessAName 
          Path = path
          Meta = Some returns<'Returns>
          Verb = GET
          Handler = handler }
        
    static member get (path: string, handler: HttpHandler, meta: FsEndpointMeta) =
        let guessAName = tryGenerateOperationName path GET
        { Name = guessAName 
          Path = path
          Meta = Some meta
          Verb = GET
          Handler = handler }

    static member post (path:string, handler: HttpHandler) =
        let guessAName = tryGenerateOperationName path GET
        { Name = guessAName 
          Path = path
          Meta = None
          Verb = POST
          Handler = handler }
                
    static member post<'Response> (path:string, handler: HttpHandler) =
        let guessAName = tryGenerateOperationName path POST
        { Name = guessAName
          Path = path
          Meta = Some returns<'Response>
          Verb = POST
          Handler = handler }
        
    static member post<'Request, 'Response> (path:string, handler: HttpHandler) =
        let guessAName = tryGenerateOperationName path POST
        { Name = guessAName
          Path = path
          Meta = Some (metaBuilder {
              returnType typeof<'Response>
              accepts typeof<'Request>
          })
          Verb = POST
          Handler = handler }
                                                
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