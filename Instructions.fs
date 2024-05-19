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
    | AND of Resource * Resource
    | OR of Resource * Resource
    | XOR of Resource * Resource
    | INC of Resource
    | DEC of Resource
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
    //ADD
    |> Map.add 0x80uy (ADD (Register A,Register B))
    |> Map.add 0x81uy (ADD (Register A,Register C))
    |> Map.add 0x82uy (ADD (Register A,Register D))
    |> Map.add 0x83uy (ADD (Register A,Register E))
    |> Map.add 0x84uy (ADD (Register A,Register H))
    |> Map.add 0x85uy (ADD (Register A,Register L))
    |> Map.add 0x86uy (ADD (Register A, (Pointer (Vregister HL))))
    |> Map.add 0x87uy (ADD (Register A,Register A))
    |> Map.add 0xC6uy (ADD (Register A, Number N8))
    |> Map.add 0x09uy (ADD (Vregister HL, Vregister BC))
    |> Map.add 0x19uy (ADD (Vregister HL, Vregister DE))
    |> Map.add 0x29uy (ADD (Vregister HL, Vregister HL))
    |> Map.add 0x39uy (ADD (Vregister HL, Register SP))
    |> Map.add 0xE8uy (ADD (Register SP, Number E8))
    // SUB
    |> Map.add 0x90uy (SUB (Register A,Register B))
    |> Map.add 0x91uy (SUB (Register A,Register C))
    |> Map.add 0x92uy (SUB (Register A,Register D))
    |> Map.add 0x93uy (SUB (Register A,Register E))
    |> Map.add 0x94uy (SUB (Register A,Register H))
    |> Map.add 0x95uy (SUB (Register A,Register L))
    |> Map.add 0x96uy (SUB (Register A,(Pointer (Vregister HL))))
    |> Map.add 0x97uy (SUB (Register A,Register A))
    |> Map.add 0xD6uy (SUB (Register A,Number N8))
    //SBC
    |> Map.add 0x98uy (SBC (Register A,Register B))
    |> Map.add 0x99uy (SBC (Register A,Register C))
    |> Map.add 0x9Auy (SBC (Register A,Register D))
    |> Map.add 0x9Buy (SBC (Register A,Register E))
    |> Map.add 0x9Cuy (SBC (Register A,Register H))
    |> Map.add 0x9Duy (SBC (Register A,Register L))
    |> Map.add 0x9Euy (SBC (Register A,(Pointer (Vregister HL))))
    |> Map.add 0x9Fuy (SBC (Register A,Register A))
    |> Map.add 0xDEuy (SBC (Register A,Number N8))
    //ADC
    |> Map.add 0x88uy (ADC (Register A,Register B))
    |> Map.add 0x89uy (ADC (Register A,Register C))
    |> Map.add 0x8Auy (ADC (Register A,Register D))
    |> Map.add 0x8Buy (ADC (Register A,Register E))
    |> Map.add 0x8Cuy (ADC (Register A,Register H))
    |> Map.add 0x8Duy (ADC (Register A,Register L))
    |> Map.add 0x8Euy (ADC (Register A,(Pointer (Vregister HL))))
    |> Map.add 0x8Fuy (ADC (Register A,Register A))
    |> Map.add 0xCEuy (ADC (Register A,Number N8))
    //AND
    |> Map.add 0xA0uy (AND (Register A,Register B))
    |> Map.add 0xA1uy (AND (Register A,Register C))
    |> Map.add 0xA2uy (AND (Register A,Register D))
    |> Map.add 0xA3uy (AND (Register A,Register E))
    |> Map.add 0xA4uy (AND (Register A,Register H))
    |> Map.add 0xA5uy (AND (Register A,Register L))
    |> Map.add 0xA6uy (AND (Register A,(Pointer (Vregister HL))))
    |> Map.add 0xA7uy (AND (Register A,Register A))
    |> Map.add 0xE6uy (AND (Register A,Number N8))
    //OR
    |> Map.add 0xB0uy (OR (Register A,Register B))
    |> Map.add 0xB1uy (OR (Register A,Register C))
    |> Map.add 0xB2uy (OR (Register A,Register D))
    |> Map.add 0xB3uy (OR (Register A,Register E))
    |> Map.add 0xB4uy (OR (Register A,Register H))
    |> Map.add 0xB5uy (OR (Register A,Register L))
    |> Map.add 0xB6uy (OR (Register A,(Pointer (Vregister HL))))
    |> Map.add 0xB7uy (OR (Register A,Register A))
    |> Map.add 0xF6uy (OR (Register A,Number N8))
    //XOR
    |> Map.add 0x98uy (OR (Register A,Register B))
    |> Map.add 0x99uy (OR (Register A,Register C))
    |> Map.add 0x9Auy (OR (Register A,Register D))
    |> Map.add 0x9Buy (OR (Register A,Register E))
    |> Map.add 0x9Cuy (OR (Register A,Register H))
    |> Map.add 0x9Duy (OR (Register A,Register L))
    |> Map.add 0x9Euy (OR (Register A,(Pointer (Vregister HL))))
    |> Map.add 0x9Fuy (OR (Register A,Register A))
    |> Map.add 0xEEuy (OR (Register A,Number N8))

