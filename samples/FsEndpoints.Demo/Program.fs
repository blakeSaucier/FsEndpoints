open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open FsEndpoints
open Giraffe.Core

type Person =
    { Id: int
      Name: string
      MiddleName: string }

type Book =
    { Title: string
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

open Giraffe

let personHandler : HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->        
        let id =  ctx.Request.RouteValues["id"].ToString() |> int
        json { Id = id; Name = "James"; MiddleName = "" } next ctx

let a = returns<Person>
        
let api = [
    endpoint {
        GET "/person/{id:int}"
        schema
            { Request = None
              Responses = [
                  { Content = ApplicationJson
                    Status = System.Net.HttpStatusCode.OK
                    Response = Some typeof<Person> }
                  { Content = ApplicationJson
                    Status = System.Net.HttpStatusCode.NotFound
                    Response = Some typeof<unit> }
              ] }
        handler personHandler
    }
    
    endpoint {
        POST "/companies"
        handler (json ())
    }
    
    endpoint {
        POST "/person/{personId:int}/book"
        handler (json { Id = 123; Name = "James"; MiddleName = "" })
    }
]

let builder = WebApplication.CreateBuilder()
builder.Services.AddGiraffe() |> ignore

let app = builder.Build()
app.UseFsEndpoints api |> ignore

app.Run()