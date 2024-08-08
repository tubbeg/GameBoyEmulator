module Opcodes
open Register

type Instruction = 
    | NOP
    | ADD
    | AND
    | SUB
    | OR
    | XOR
    | ADC
    | SBC
    | LD
    | INC
    | DEC
    | HALT

type Operand = 
    | A
    | B
    | C
    | D
    | E
    | F
    | H
    | L
    | SP
    | AF
    | BC
    | DE 
    | HL
    | N8
    | A8
    | N16
    | Pointer of Operand

type OperandParse =
    {
        instruction:Instruction;
        operand1:Operand option;
        operand2:Operand option;
    }


let createOpRecord ins op1 op2 =
    Some {instruction=ins;operand1=Some op1;operand2=Some op2}

let parseOpcode (code : Byte) : OperandParse option =
    match code.byteValue with
    | 0 -> Some {instruction=NOP;operand1=None;operand2=None}
    | 0x01 -> createOpRecord LD BC N16
    | 0x80 -> createOpRecord ADD A B
    | 0x81 -> createOpRecord ADD A C
    | _ ->  None