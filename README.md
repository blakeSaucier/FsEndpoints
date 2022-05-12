# FsEndpoints
Lightweight restful API for F#

```f#
let api = [
    endpoint {
        GET "/person/{id:int}"
        response [
            OK, returns<Person>
        ]
        handler getPersonHandler
    }
]
```

## Another Web Framework?
The F# ecosystem has some great web frameworks: Giraffe, Falco, Suave. These projects have one thing in common: each describes an endpoint as a set of functions.   

There is nothing wrong with this approach, after all, this is a tenant of functional programming.

When doing functional programming, we like to solve problems by breaking them down into *functions* and *data*.

This framework allows us to describe web endpoints as *Data*.

By taking this approach we get some interesting benefits, one of which is the ability to generate *documentation*.

## Features
- ✅ Describe an HTTP endpoint with idiomatic F#
- ✅ Built on top of Asp Net Core
- ✅ Automatic Open Api/Swagger generation

## Roadmap
- Automatic serialization/deserialization
- Validation
- Moving away from Giraffe based HttpHandlers
- Authentication
