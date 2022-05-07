[<AutoOpen>]
module FsEndpoints.CE

open System
open FsEndpoints
open Microsoft.AspNetCore.Http

let defaultMeta =
    { Content = ApplicationJson
      Response = None
      Request = None }

type EndpointMetaBuilder () =
    member _.Yield _ =  defaultMeta
        
    member _.Run (s: FsEndpointMeta) = s
                    
    [<CustomOperation("returnType")>]
    member _.Returns (state: FsEndpointMeta, t: Type) =
        { state with Response = Some t }
        
    [<CustomOperation("accepts")>]
    member _.Accepts (state: FsEndpointMeta, t: Type) =
        { state with Request = Some t }
        
    [<CustomOperation("content")>]        
    member _.Content (state: FsEndpointMeta, content: MediaType) =
        { state with Content = content }

let metaBuilder = EndpointMetaBuilder()

let schemaBuilder = EndpointMetaBuilder()

let returns<'t> = metaBuilder {
    returnType typeof<'t>
}

let jsonSchema<'request, 'response> = metaBuilder {
    content ApplicationJson
    returnType typeof<'request>
    accepts typeof<'response>
}

let notImplemented : Giraffe.Core.HttpHandler =
    fun (_: Giraffe.Core.HttpFunc) (_: HttpContext) -> failwith "not implemented"

let defaultSchema =
    { Responses = []
      Request = None }

let defaultEndpoint =
    { Name = ""
      Path = "/"
      Schema = None
      Verb = GET
      Handler = notImplemented }
    
let a<'t> = typeof<'t>

type EndpointBuilder () =
    member _.Yield _ = defaultEndpoint
    member _.Run (s: FsEndpointDef) = s
    
    /// Http Get endpoint
    [<CustomOperation("GET")>]
    member _.Get (state: FsEndpointDef, path: string) =
        { state with
            Name = tryGenerateOperationName path GET
            Verb = GET
            Path = path }
        
    /// Http Post endpoint
    [<CustomOperation("POST")>]
    member _.Post (state: FsEndpointDef, path: string) =
        { state with
            Name = tryGenerateOperationName path POST
            Verb = POST
            Path = path }
        
    /// Http Post endpoint
    [<CustomOperation("PUT")>]
    member _.Put (state: FsEndpointDef, path: string) =
        { state with
            Name = tryGenerateOperationName path PUT
            Verb = PUT
            Path = path }
    
    [<CustomOperation("description")>]
    member _.Description (state: FsEndpointDef, description: string) =
        { state with Name = description }
        
    [<CustomOperation("handler")>]
    member _.Handler (state: FsEndpointDef, handler: Giraffe.Core.HttpHandler) =
        { state with Handler = handler }
    
    [<CustomOperation("schema")>]
    member _.Schema (state: FsEndpointDef, schema: FsEndpointSchema) =
        { state with Schema = Some schema }
        
let endpoint = EndpointBuilder()   