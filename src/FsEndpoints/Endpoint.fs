module FsEndpoints.Endpoint

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Routing
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing.Patterns
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open System.Net
open Microsoft.FSharp.Core.Printf

module Path =
    type ParamType =
        | Int | String | Bool
        
    let i = Int
    let s = String
    let b = Bool

    type Component =
        | Literal of string
        | Route of Component list
        | Param of string * ParamType
        
    let param s t = Param (s, t)    
    let literal s = Literal s
    
    type Route = Component list
                
    let (/) (c1: Component) (c2: Component) =
        Route [c1; c2]
        
    let (&?) (s: string) (t: ParamType) =
        Param (s, t)
   
    let test = literal "home" / literal "person" / literal "asdf" / ("name" &? s)
    
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

type Person = { A: string }

let returns<'t> = metaBuilder {
    returnType typeof<'t>
}

let accepts<'t> = metaBuilder {
    accepts typeof<'t>
}

let a = returns<Person>

let tryGenerateOperationName (path: string) (verb: HttpVerb) =
    let root =
        path.Split("/")
        |> Array.toList
        |> function
        | [ "" ] -> "/"
        | "" :: tail -> tail.Head
        | _ -> "/"
    $"{verb} {root}"
    
let private tryGetParser (c : string) =
    let decodeSlashes (s : string) = s.Replace("%2F", "/").Replace("%2f", "/")
    let parseGuid     (s : string) =
        match s.Length with
        | 22 -> ShortGuid.toGuid s
        | _  -> Guid s

    match c with
    | "string" -> Some (decodeSlashes    >> box)
    | "int" -> Some (int              >> box)
    | "bool" -> Some (bool.Parse       >> box)
    | "char" -> Some (char              >> box)
    | "double" -> Some (int64            >> box)
    | "float" -> Some (float            >> box)
    | "guid" -> Some (parseGuid        >> box)
    | "short" -> Some (ShortId.toUInt64 >> box)
    | _   -> None

let stripSprintf (s: string) =
    s.Replace("%i", "int")
        .Replace("%s", "string")
        .Replace("%d", "long")
        .Replace("%b", "bool")
    
let parsePath (path : PrintfFormat<_,_,_,_, 'T>) =
    let cleaned = stripSprintf path.Value
    let parsed = RoutePatternFactory.Parse(cleaned)
    let parameters =
        parsed.Parameters
        |> Seq.map (fun a ->
            a.Name, a.ParameterPolicies[0].Content)
        |> Seq.toList
    cleaned, parameters

let private convertToTuple (mappings : (string * string) list) (routeData : RouteData) =
    let values =
        mappings
        |> List.map (fun (placeholderName, paramType) ->
            let routeValue = routeData.Values.[placeholderName]
            match tryGetParser paramType with
            | Some parseFn -> parseFn (routeValue.ToString())
            | None         -> routeValue)
        |> List.toArray

    let result =
        match values.Length with
        | 1 -> values.[0]
        | _ ->
            let types =
                values
                |> Array.map (fun v -> v.GetType())
            let tupleType = FSharpType.MakeTupleType types
            FSharpValue.MakeTuple(values, tupleType)
    result
    
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
        
let createHandler (mappings : (string * string) list) (handler : 'T -> HttpHandler) =
    fun (ctx: HttpContext) ->
        let tuple =
            ctx.GetRouteData()
            |> convertToTuple mappings
            :?> 'T
        handler tuple
          
let routef
    (name: string)
    (path         : PrintfFormat<_,_,_,_, 'T>)
    (routeHandler : 'T -> HttpHandler)
    (config: FsEndpointMeta option)
    : FsEndpointDef =
    let newTemplate, newMappings = parsePath path
    let appliedHandler = createHandler newMappings routeHandler
    
    let myHandler : HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            appliedHandler ctx next ctx
            
    { Name = name
      Path = newTemplate.Replace(":string", "") // Asp complains when a path param contains :string
      Meta = config
      Verb = GET
      Handler = myHandler }
    
module FsEndpoints =
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
                                                
let addEndpoint (app: WebApplication) (e: FsEndpointDef) =
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