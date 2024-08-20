open Browser
open Register
open Processor
open Fetch
open ALU
open Opcodes
open FSharp.Data
open Fable.Core
let b = 100

let mutable a = 255

a <- ( a ||| b) //load b into a
a <- a &&& b //mask unnecessary bits


let c = toHexString (N 3,N 2)




let testJsonRecordsUnprefixed prefix =
    let iter = 0xFF
    let init = 0
    let rec testRecords i  =
        let b = intToByte i
        match b,i with
        | Some d, j when (j <= iter) ->
            match jsonToRecord d prefix  with
            | Some r ->
                printfn "Recieved result %A" r
                testRecords (i + 1) 
            | _ ->
                eprintfn "Failed at code %A" i
        | _,j when (j >= iter) -> printfn "Finished"
        | _ -> eprintfn "Failed to parse byte"
    testRecords init 
    testRecords init 

testJsonRecordsUnprefixed true
testJsonRecordsUnprefixed false


let div = document.createElement "div"
div.innerHTML <- "hello"
document.body.appendChild div |> ignore

