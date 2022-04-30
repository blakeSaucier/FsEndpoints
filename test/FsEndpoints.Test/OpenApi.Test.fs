module FsEndpoints.Test.OpenApi

open System
open System.Collections.Generic
open Xunit
open FsEndpoints.OpenApi
open OpenApi
open FsUnit
open Generate
open Expecto

type Person =
    { First: string
      Last: string }
    
type Company =
    { Name: string
      Revenue: decimal
      Employees: Person list }
    
module Reflection =
    open Reflection
    open Models
    
    [<Fact>]
    let ``Should identify if it's a list`` () =
        isList typeof<int list>
        |> should equal true
        
        isList typeof<int[]>
        |> should equal false
        
        isList typeof<Person>
        |> should equal false
        
        isList typeof<Map<int, string>>
        |> should equal false
        
    [<Fact>]
    let ``Should identify if it's an array`` () =
        isArray typeof<int list>
        |> should equal false
        
        isArray typeof< int[]>
        |> should equal true
        
        isArray typeof<Person>
        |> should equal false
        
    [<Fact>]
    let ``Should identify if it's a record`` () =
        isRecord typeof<int list>
        |> should equal false
        
        isRecord typeof< int[]>
        |> should equal false
        
        isRecord typeof<Person>
        |> should equal true
        
    [<Fact>]
    let ``Should convert simple record to ApiSchemaTypes`` () =
        let res = toOpenApiDomain typeof<Person>
        
        let expected =
            Object [
                "First", String NoFormat
                "Last", String NoFormat
            ]
        res |> should equal expected
        
    [<Fact>]
    let ``Should convert Array to ApiSchemaType`` () =
        let res = toOpenApiDomain typeof<int[]>
        
        let expected =
            Array (Int Int32)
            
        res |> should equal expected
        
    [<Fact>]
    let ``Should convert List to ApiSchemaType`` () =
        let res = toOpenApiDomain typeof<string list>
        
        let expected = Array (String NoFormat)
            
        res |> should equal expected
        
    [<Fact>]
    let ``Should convert List of records to ApiSchemaType`` () =
        let res = toOpenApiDomain typeof<Person list>
        let res2 = toOpenApiDomain typeof<Person array>
        
        let expected =
            Array
                (Object
                     [ "First", String NoFormat
                       "Last", String NoFormat ])
            
        res |> should equal expected
        res2 |> should equal expected
        
    [<Fact>]
    let ``Should convert complex object to ApiSchemaType`` () =
        let res = toOpenApiDomain typeof<Company list>
        
        let expected =
            Array (Object
              [ "Name", String NoFormat;
                "Revenue", Number Float
                "Employees", Array (Object
                  [ "First", String NoFormat
                    "Last", String NoFormat ]) ])
            
        Expect.equal res expected "Should be the same"
    
module OpenApi =
    open Reflection
    open Models
    open ObjectsComparer

    let comparer = Comparer()
    let printDiffs (diffs: IEnumerable<Difference>) =
        diffs |> Seq.toList |> sprintf "%A"
    
    [<Fact>]
    let ``Should convert domain to OpenApiSchema`` () =
        let input =
            Array
                (Object
                     [ "first", String NoFormat
                       "last", String NoFormat ])
        let res = toOpenApiSchema input
        
        let expected =
            apiSchema {
                schemaType "array"
                Items (apiSchema {
                    schemaType "object"
                    properties [
                        "first", apiSchema {
                            schemaType "string"
                            format "string"
                        }
                        "last", apiSchema {
                            schemaType "string"
                            format "string"
                        }
                    ]
                })
            }
            
        let areEqual, diffs = comparer.Compare(res, expected)
        Expect.isTrue areEqual (printDiffs diffs)
    
    [<Fact>]
    let ``Should convert with formatting`` () =
        let input =
          Array (Object
            [ "Name", String NoFormat;
              "Revenue", Number Float
              "Employees", Array (Object
                [ "First", String NoFormat
                  "Last", String NoFormat
                  "Salary", Number Float ]) ])
        
        let res = toOpenApiSchema input
        
        let expected =
            apiSchema {
                schemaType "array"
                Items (apiSchema {
                    schemaType "object"
                    properties [
                        "name", apiSchema {
                            schemaType "string"
                            format "string"
                        }
                        "revenue", apiSchema {
                            schemaType "number"
                            format "float"
                        }
                        "employees", apiSchema {
                            schemaType "array"
                            Items (apiSchema {
                                schemaType "object"
                                properties [
                                    "first", apiSchema {
                                        schemaType "string"
                                        format "string"
                                    }
                                    "last", apiSchema {
                                        schemaType "string"
                                        format "string"
                                    }
                                    "salary", apiSchema {
                                        schemaType "number"
                                        format "float"
                                    }
                                ]
                            })
                        }
                    ]
                })
            }
            
        let areEqual, diffs = comparer.Compare(res, expected)
        Expect.isTrue areEqual (printDiffs diffs)
    
    [<Fact>]    
    let ``Should convert a type to OpenApiSchema`` () =
        let res =
            typeof<Person>
            |> toOpenApiDomain
            |> toOpenApiSchema
        
        let expected =
            apiSchema {
                schemaType "object"
                properties [
                    "first", apiSchema {
                        schemaType "string"
                        format "string"
                    }
                    "last", apiSchema {
                        schemaType "string"
                        format "string"
                    }
                ]
            }
        ()
        
        let areEqual, diffs = comparer.Compare(res, expected)
        Expect.isTrue areEqual (printDiffs diffs)    