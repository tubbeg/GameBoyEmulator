module Processor
open Memory
open Opcodes
open Register
open ALU
open Instruction
open Fetch

let halt () =
    printfn "Detected HALT instruction"
    None,None

let executeInstruction (parse : OperandParse) registers m : Registers option * Memory option  =
    let f = fetch parse registers m
    match f,parse.instruction with
    | Some fetch,ADD -> addInstruction parse fetch,Some m
    | Some fetch,SUB -> subInstruction fetch,Some m
    | _,HALT -> halt ()
    | _,_ -> None,None

let execute memory registers  =
    let rec exec m  r  =
        match getDataFromMem r.PC m with
        | Some n ->
            let parse = parseOpcode n
            match parse with
            | Some p ->
                match executeInstruction p r m with
                | Some updatedRegister,Some  updatedMemory ->
                    exec updatedMemory (incPC updatedRegister)
                | _ -> None,None
            | _ -> None,None
        | _ -> None,None
    exec memory registers