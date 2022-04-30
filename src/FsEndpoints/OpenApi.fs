module FsEndpoints.OpenApi

open System
open System.Net
open FsEndpoints.Endpoint
open System.IO
open Microsoft.AspNetCore.Routing.Patterns
open Microsoft.FSharp.Reflection
open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Writers
open OpenApi

module Models =

  type OpenApiNumberFormat = Float | Double
        
  type OpenApiIntFormat = Int32 | Int64
        
  type OpenApiStringFormat = Date | DateTime | Password | Byte | Binary | Uuid | NoFormat

  type OpenApiSchemaType =
    | Int of OpenApiIntFormat
    | String of OpenApiStringFormat
    | Bool
    | Number of OpenApiNumberFormat
    | Array of OpenApiSchemaType
    | Object of (string * OpenApiSchemaType) list
        
  let primitiveTypeMap = dict [
    typeof<int16>, Int Int32
    typeof<int32>, Int Int32
    typeof<int64>, Int Int64
    typeof<decimal>, Number Float
    typeof<double>, Number Double
    typeof<float>, Number Float
    typeof<string>, String NoFormat
    typeof<DateTime>, String DateTime
    typeof<DateOnly>, String Date
    typeof<Guid>, String Uuid
    typeof<unit>, String NoFormat
  ]
    
module Reflection =
  open Models

  let getPrimitiveOpenApiType (t: Type) =
    match primitiveTypeMap.TryGetValue(t) with
    | true, value -> Some value
    | false, _ -> None

  let isPrimitive (t: Type) =
    getPrimitiveOpenApiType t
    |> Option.isSome
      
  let isList (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<List<_>>

  let isOption (t: Type): bool =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

  let getOptionType (t: Type): Type =
    t.GetGenericArguments().[0]

  let getListType (itemType: Type) =
    typedefof<List<_>>.MakeGenericType([| itemType |])

  let getListItemType (t: Type) =
    t.GetGenericArguments()[0]

  let isMap (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_,_>>

  let isArray (t: Type) = t.IsArray

  let arrayItemType (t: Type) =
    t.GetElementType()

  let isRecord = FSharpType.IsRecord

  let getRecordProperties (t: Type) =
    if (isRecord t) then
      t.GetProperties()
      |> Array.map (fun p -> p.Name, p.PropertyType)
    else
      Array.empty
              
  let (|List|Array|Record|Primitive|) (t: Type) =
    match t with
    | t when isList t -> List
    | t when isArray t -> Array
    | t when isRecord t -> Record
    | t when isPrimitive t -> Primitive
    | t -> failwith $"unsupported type: {t.Name}"
        
  let rec toOpenApiDomain (t: Type) : OpenApiSchemaType =
    match t with
    | List ->
      let listSubType = getListItemType t
      let schema = toOpenApiDomain listSubType
      Array schema
    | Array ->
      let arrItemType = arrayItemType t
      let schema = toOpenApiDomain arrItemType
      Array schema
    | Record ->
      getRecordProperties t
      |> Array.map (fun (name, type') -> name, (toOpenApiDomain type'))
      |> Array.toList
      |> Object
    | Primitive ->
      getPrimitiveOpenApiType t
      |> Option.defaultWith (fun () -> failwith $"unknown type {t.Name}")

module Generate =
  open ExtendedTypes
  open Reflection
  open Models
    
    /// Open api does not expect the 'type' part of the path parameter
  let stripTypesFromPathParams (path: string) =
    path
    |> String.remove ":string"
    |> String.remove ":int"
    |> String.remove ":int16"
    |> String.remove ":int32"
    |> String.remove ":int64"
    |> String.remove ":number"
    |> String.remove ":bool"
    |> String.remove ":"

  let dotnetToOpenApi = Map [
    "int", "integer"
    "int16", "integer"
    "int32", "integer"
    "int64", "integer"
    "string", "string"
    "datetime", "string"
    "dateonly", "string"
    "guid", "string"
    "bool", "boolean"
    "boolean", "boolean"
    "float", "number"
    "decimal", "number"
    "double", "number"
  ]
    
  let dataTypeToString (domain: OpenApiSchemaType) =
    match domain with
    | Int _ -> "integer"
    | String _ -> "string"
    | Bool -> "boolean"
    | Number _ -> "number"
    | Array _ -> "array"
    | Object _ -> "object"
        
  let intFormatToString (intFormat: OpenApiIntFormat) =
    match intFormat with
    | Int32 -> "int32"
    | Int64 -> "int64"
        
  let numberFormatToString (numberFormat: OpenApiNumberFormat) =
    match numberFormat with
    | Double -> "double"
    | Float -> "float"
        
  let stringFormatToString (stringFormat: OpenApiStringFormat) =
    match stringFormat with
    | Binary -> "binary"
    | Byte -> "byte"
    | Date -> "date"
    | Password -> "password"
    | DateTime -> "date-time"
    | NoFormat -> "string"
    | Uuid -> "uuid"

    /// convert the dotnet types to their openapi equivalent
  let convertToOpenApiTypes (t: string) =
      match Map.tryFind t dotnetToOpenApi with
      | Some s -> s
      | None -> failwith $"unknown type {t}"

  /// use camel case for open api property names
  let toCamelCase (name: string) =
      if (String.IsNullOrWhiteSpace(name)) then name
      else
          let firstChar = Char.ToLowerInvariant(name[0])
          $"{firstChar}{name.Substring(1)}"
          
  let rec toOpenApiSchema (domain: OpenApiSchemaType) : OpenApiSchema =
    match domain with
    | Int intFormat ->
      apiSchema {
        schemaType (dataTypeToString domain)
        format (intFormatToString intFormat)
      }
    | String stringFormat ->
      apiSchema {
        schemaType (dataTypeToString domain)
        format (stringFormatToString stringFormat)
      }
    | Bool ->
      apiSchema {
        schemaType (dataTypeToString domain) 
      }
    | Number numberFormat ->
      apiSchema {
        schemaType (dataTypeToString domain)
        format (numberFormatToString numberFormat)
      }
    | Array openApiSchemaType ->
      apiSchema {
        schemaType "array"
        Items (toOpenApiSchema openApiSchemaType)
      }
    | Object props ->
      let subSchemas =
        props |> List.map (fun (name, type') -> toCamelCase name, (toOpenApiSchema type'))
      apiSchema {
        schemaType "object"
        properties subSchemas
      }

  let generatePath (endpoint: FsEndpointDef) =
    let verb = match endpoint.Verb with
                | GET -> OperationType.Get
                | POST -> OperationType.Post
                | PUT -> OperationType.Put
                
    let route = RoutePatternFactory.Parse(endpoint.Path).Parameters
      
    let typeConstraint (p : RoutePatternParameterPolicyReference seq) =
        p
        |> Seq.toList
        |> function
            | [ t ] -> t.Content
            | _ -> "string"
            
    let generateSchema (t: Type) =
      let openApiSchema = t |> toOpenApiDomain |> toOpenApiSchema
      "application/json", apiMediaType {
          schema openApiSchema
      }
      
    let generateRequestContent (endpoint: FsEndpointDef) =
      match endpoint.Meta with
      | Some c ->
        match c.Request with
        | Some r -> [ generateSchema r ]
        | _ -> [] 
      | _ -> []
          
    let generateReturnContent (endpoint: FsEndpointDef) =
      match endpoint.Meta with
      | Some c ->
        match c.Response with
        | Some r -> [ generateSchema r ]
        | _ -> []
      | _ -> []
      
    let generatePathParameters (parameters: RoutePatternParameterPart seq) =
      parameters
      |> Seq.map (fun p ->
        let name = p.Name
        let pType = typeConstraint p.ParameterPolicies
        name, pType)
      |> Seq.map (fun (n, t) ->
        apiParameter {
          name n
          location ParameterLocation.Path
          required true
          schema (apiSchema {
            schemaType (convertToOpenApiTypes t)
          })
        })
      
    (stripTypesFromPathParams endpoint.Path), apiPathItem {
        operations [
          verb, apiOperation {
            tags [
              apiTag {
                name endpoint.Name
              }
            ]
            parameters [
              yield! (generatePathParameters route)
            ]
            
            requestBody (apiRequestBody {
              content (generateRequestContent endpoint)
            })
            description endpoint.Name
            responses [HttpStatusCode.OK, apiResponse {
              description "Success"
              content (generateReturnContent endpoint)
            }]
          }
        ]
    }
    
  let generateOpenApiDocument (endpoints: FsEndpointDef list) =
    let p = endpoints |> Seq.map generatePath
    apiDocument {
      info (apiInfo {
        version "1.0.0"
        title "TBD"
      })
      paths p
    }

module Write =
  let writeSpec (doc: OpenApiDocument) =
    use writer = new StringWriter()
    let a = OpenApiJsonWriter(writer)
    doc.SerializeAsV3(a)
    writer.ToString()