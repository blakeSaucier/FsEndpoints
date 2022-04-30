open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open FsEndpoints
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

open Giraffe

let personHandler : HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->        
        let id =  ctx.Request.RouteValues["id"].ToString() |> int
        json { james with Id = id } next ctx
        
let api = [
    Endpoint.get<Person>("/person/{id:int}", personHandler)
    Endpoint.post<Company, unit>("/companies", json ())
    Endpoint.post<Book, Person[]>("/person/{personId:int}/book", json { Name = "James"; MiddleName = "Test"; Id = 1234 })
]

let builder = WebApplication.CreateBuilder()
builder.Services.AddGiraffe() |> ignore

let app = builder.Build()
app.UseFsEndpoints api |> ignore

app.Run()