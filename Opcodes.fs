module Opcodes
open Register

type Instruction = 
    | NOP
    | ADD
    | SUB
    | OR
    | XOR
    | ADC
    | SBC
    | LD

type Operand = 
    | A
    | B
    | C
    | D
    | E
    | AF
    | BC
    | DE 
    | HL
    | N8
    | A8
    | N16
    | Pointer of Operand

let parseOpcode (code : Byte) =
    match code.byteValue with
    | 0 -> Some NOP,None,None
    | 0x01 -> Some LD, Some BC, Some N16
    | 0x80 -> Some ADD,Some A,Some B
    | 0x81 -> Some ADD,Some A,Some C
    | _ ->  None,None,None