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



//READ THIS



//WHAT HAPPENED LAST TIME?


//* MANAGED TO FIGURE OUT LESS STUPID WAY TO GET OPCODES

//* CHANGED TO 4 BIT IMPLEMENTATION. ARITHMETIC NEEDS TO BE TESTED, BUT SHOULD WORK

//* FOUND THE ERROR IN THE OPCODES. FORGOT ["operands"] SO ITS OK. NEEDS A LITTLE FIXING


let testJsonRecords () =
    let iter = 0xFFFF
    let init = 0
    let rec testRecords i =
        let b = intToByte i
        match b,i with
        | Some d,_ ->
            match jsonToRecord d with
            | Some r ->
                printfn "Recieved result %A" r
                testRecords (i + 1)
            | _ ->
                eprintfn "Failed at code %A" i
        | _,j when (j = iter) -> printfn "Finished"
        | _ -> eprintfn "Failed to parse byte"
    testRecords init


testJsonRecords()

let div = document.createElement "div"
div.innerHTML <- "hello"
document.body.appendChild div |> ignore

