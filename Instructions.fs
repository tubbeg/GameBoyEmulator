module Instructions
open Registers

let add n n2 =
    n + n2


type Immediate =
    | A8
    | N8
    | E8
    | A16
    | N16

type Resource =
    | Number of Immediate
    | Register of GameBoyRegister
    | Vregister of VirtualRegister
    | Pointer of Resource

    
type Instruction =
    | ADD of Resource * Resource
    | SUB  of Resource * Resource
    | ADC of Resource * Resource
    | SBC of Resource * Resource
    | LD of Resource * Resource
    | NOP

let instr : Instruction = (ADD (Register A,Register B))

let res = A,B

let res2 = ADD (Register A, (Pointer (Vregister HL)))

let InstructionOpCodeMap : Map<byte,Instruction> =
    Map.empty
    |> Map.add 0x00uy NOP 
    |> Map.add 0x80uy (ADD (Register A,Register B))
    |> Map.add 0x81uy (ADD (Register A,Register C))
    |> Map.add 0x82uy (ADD (Register A,Register D))
    |> Map.add 0x83uy (ADD (Register A,Register E))
    |> Map.add 0x84uy (ADD (Register A,Register H))
    |> Map.add 0x85uy (ADD (Register A,Register L))
    |> Map.add 0x86uy (ADD (Register A,Register C))
    |> Map.add 0x87uy (ADD (Register A, (Pointer (Vregister HL))))
    |> Map.add 0xC6uy (ADD (Register A, Number N8))
    |> Map.add 0x09uy (ADD (Vregister HL, Vregister BC))
    |> Map.add 0x19uy (ADD (Vregister HL, Vregister DE))
    |> Map.add 0x29uy (ADD (Vregister HL, Vregister HL))
    |> Map.add 0x39uy (ADD (Vregister HL, Register SP))
    |> Map.add 0xE8uy (ADD (Register SP, Number E8))

