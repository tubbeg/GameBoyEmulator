module Processor
open Memory
open Opcodes
open Register
open ALU
open Instruction
open Fetch


//READ THIS
//What happened last time?
(*

* Haven't tested signed bytes to confirm it works. I think it should, but not certain.

* Working on adding all opcodes

* ALU operations are working so just need to add instructions in the module

* After arithmetic operations I will need to start working on instructions like LD,CP for instance.

*)






type ProcessorData =
    Memory * Registers

let halt () =
    printfn "Detected HALT instruction"
    None

let initialRegisters =
    {A=Byte 0;B=Byte 0;C=Byte 0;D=Byte 0;F= Byte 0;E=Byte 0; H= Byte 0; L = Byte 0; SP = Short 0; PC = Short 0}

let toProcessorData  (mem : Memory) (r : Registers option) : ProcessorData option =
    //type transformation. This function is actually redundant, but the it makes the module
    //much cleaner and easier to read.
    match r,mem with
    | Some ur,mem -> Some (mem,ur)
    | _ -> None

let executeInstruction registers m (p : OperandParse option) : ProcessorData option  =
    match p with
    | Some parse ->
        let f = fetch parse registers m
        match f,parse.instruction with
        | Some fetch,ADD -> addInstruction parse fetch |> toProcessorData m
        | Some fetch,SUB -> subInstruction fetch m |> toProcessorData m
        | _,HALT -> halt ()
        | _,_ -> None
    | _ -> None

let incrementPC rm : ProcessorData option =
    match rm with
    | Some (m,r) ->
        Some (m,incPC r)
    | _ -> None

let execute memory  =
    let ir = initialRegisters
    let rec exec (rm : ProcessorData option)  =
        match rm with
        | Some (m,r) ->
            getDataFromMem r.PC m |>
            parseOpcode |>
            executeInstruction r m |>
            incrementPC |>
            exec
        | _ -> None
    exec (Some (memory,ir))