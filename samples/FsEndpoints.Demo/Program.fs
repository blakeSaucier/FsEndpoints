open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open FsEndpoints
open Endpoint
open Giraffe.Core

type Person = { Id: int
                Name: string
                MiddleName: string }

type Book = { Title: string
              AuthorId: int }

type Employee =
    { First: string
      Last: string
      HireDate: DateTime
      Id: int64
      Salary: Decimal }
    
type Company =
    { Name: string
      Id: Guid
      Budget: Decimal
      YearEnd: DateOnly
      Employees: Employee list }
    
let james =
    { Id = 123
      Name = "James"
      MiddleName = "Blake" }

let endpoint2 : FsEndpointDef =
    { Name = "Person"
      Path = "/person"
      Meta = Some { OperationName = "GetPerson"; Produces = ApplicationJson; Response = Some typeof<Person>; Request = None }
      Verb = Endpoint.GET
      Handler = (json { Id = 1; Name = "James"; MiddleName = "Blake" }) }
    
let myhandler (testId, authorId: string) =
    json { Id = testId; Name = authorId; MiddleName = "Blake" }
    
let endpoint3 =
    routef "MyEndpoint" "/test/{id:%i}/author/{name:%s}" myhandler

open Giraffe

let personHandler : HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->        
        let id =  ctx.Request.RouteValues["id"].ToString() |> int
        json { james with Id = id } next ctx

open type FsEndpoints.Endpoint

let okJson<'t> (t: 't) : HttpHandler =
    setStatusCode 200 >=> json t 

let aPerson (model: 't) : HttpHandler =
    okJson<'t> model
    
open type FsEndpoints.Endpoint

let api = [
    get("/", json {| Data = "test" |})
    get<Person>("/person/{id:int}", personHandler)
    get<{| Name: string |}>("testing", json ())
    post<Company, unit>("/companies", json ())
    post<Book, Person[]>("/person/{personId:int}/book", json { Name = "James"; MiddleName = "Test"; Id = 1234 })
]

let doc = OpenApi.Generate.generateOpenApiDocument api
let final = OpenApi.Write.writeSpec doc

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    builder.Services.AddGiraffe() |> ignore
    let app = builder.Build()
    
    app.UseOpenApi() |> ignore
    
    app.UseSwaggerUi3(fun config -> config.DocumentPath <- "/swagger.json") |> ignore
    addEndpoints app api |> ignore
        
    app.Map("/swagger.json", (fun (ctx:HttpContext) ->
        ctx.Response.ContentType <- "application/json"
        ctx.Response.WriteAsync(final)
        )) |> ignore
    app.Run()
    0

