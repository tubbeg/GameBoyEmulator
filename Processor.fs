module Processor
open Memory
open Opcodes
open Register
open ALU
open Instruction
open Fetch

let executeInstruction (parse : OperandParse) registers m  =
    let f = fetch parse registers m
    match f,parse.instruction with
    | Some fetch,ADD -> addInstruction fetch
    | _,_ -> None


let execute memory registers : int option =
    let rec exec (data )  =
        match getDataFromMem registers.PC memory with
        | Some n ->
            let parse = parseOpcode n
            match parse with
            | Some ins ->
                printfn "Found instruction %A" ins
                Some 0
            | _ -> error parse
        | _ -> error data
    None