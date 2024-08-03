module Processor
open Memory
open Opcodes
open Register

let error msg = 
    eprintfn "Not found: %A" msg
    None


let alu (a : Byte) (b : Byte) (s : Status) ins =
    match ins with
    | ADD ->
        let result = a.byteValue + b.byteValue
        Byte result |> Some
    | _ -> None


let execute (m : Memory) (r : Registers) =
    let rec exec m (r: Registers) =
        let data = getDataFromMem r.PC m
        match data with
        | Some n ->
            let i,op1,op2 = parseOpcode n
            match i,op1,op2 with
            | Some ins, Some oper1,Some oper2 ->
                printfn "Found instruction %A" ins
                Some 0
            | _ -> error i 
        | _ -> error data
    None