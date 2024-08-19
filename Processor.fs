module Processor
open Memory
open Opcodes
open Register
open ALU
open Instruction


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

let execute memory  = 0