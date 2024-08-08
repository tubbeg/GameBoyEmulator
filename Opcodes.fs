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
    | RES
    | SET
    | PUSH
    | SWAP
    | RLC
    | POP
    | CPL 
    | CCF
    | RRCA
    | RRA
    | CALL
    | CP
    | PREFIX


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
    | E8
    | A8
    | N16
    | Pointer of Operand
    | PC //not actually an operand
    | Constant of Byte

type OperandParse =
    {
        instruction:Instruction;
        operand1:Operand option;
        operand2:Operand option;
    }


let opRecord2operands ins op1 op2 =
    Some {instruction=ins;operand1=Some op1;operand2=Some op2}

let opRecord1operand ins op1  =
    Some {instruction=ins;operand1=Some op1;operand2=None}

let opRecordNooperand ins   =
    Some {instruction=ins;operand1=None;operand2=None}


let adc = opRecord2operands ADC
let sbc = opRecord2operands SBC
let inc = opRecord1operand INC
let ld = opRecord2operands LD
let sub = opRecord2operands SUB
let add = opRecord2operands ADD
let dec = opRecord1operand DEC
let andd = opRecord2operands AND
let orr = opRecord2operands OR
let xor = opRecord2operands XOR
let cp = opRecord2operands CP


let parseOpPrefix c = None

let parseOpcode (c : Byte option) : OperandParse option =
    match c with
    | Some code ->
        match code.byteValue with
        | 0 -> opRecordNooperand NOP
        //PREFIX
        | 0xCB -> opRecordNooperand PREFIX
        //HALT
        | 0x76 -> opRecordNooperand HALT
        //LD
        | 0x01 -> ld BC N16
        | 0x02 -> ld (Pointer BC) A
        //INC
        | 0x03 -> inc BC
        | 0x13 -> inc DE
        | 0x23 -> inc HL
        | 0x33 -> inc SP
        | 0x04 -> inc B
        | 0x14 -> inc D
        | 0x24 -> inc H
        | 0x34 -> inc (Pointer H)
        | 0x0C -> inc C
        | 0x1C -> inc E
        | 0x2C -> inc L
        | 0x3C -> inc A
        //DEC
        | 0x05 -> dec B
        | 0x15 -> dec D
        | 0x25 -> dec H
        | 0x35 -> dec (Pointer HL)
        | 0x0B -> dec BC
        | 0x1B -> dec DE
        | 0x2B -> dec HL
        | 0x3B -> dec SP
        | 0x0D ->  dec C
        | 0x1D ->  dec E
        | 0x2D ->  dec L
        | 0x3D ->  dec A
        //ADD
        | 0x80 -> add A B
        | 0x81 -> add A C
        | 0x82 -> add A D
        | 0x83 -> add A E
        | 0x84 -> add A H
        | 0x85 -> add A L 
        | 0x86 -> add A (Pointer HL) 
        | 0x87 -> add A A 
        | 0x09 -> add HL BC 
        | 0x19 -> add HL DE 
        | 0x29 -> add HL HL 
        | 0x39 -> add HL SP
        | 0xE8 -> add SP E8
        | 0xC6 -> add A N8
        //SUB
        | 0x90 -> sub A B
        | 0x91 -> sub A C
        | 0x92 -> sub A D
        | 0x93 -> sub A E
        | 0x94 -> sub A H
        | 0x95 -> sub A L
        | 0x96 -> sub A (Pointer HL)
        | 0x97 -> sub A A
        | 0xD6 -> sub A N8
        //AND
        | 0xA0 -> andd A B
        | 0xA1 -> andd A C
        | 0xA2 -> andd A D
        | 0xA3 -> andd A E
        | 0xA4 -> andd A H
        | 0xA5 -> andd A L
        | 0xA6 -> andd A (Pointer HL)
        | 0xA7 -> andd A A
        | 0xE6 -> andd A N8
        //OR
        | 0xB0 -> orr A B
        | 0xB1 -> orr A C
        | 0xB2 -> orr A D
        | 0xB3 -> orr A E
        | 0xB4 -> orr A H
        | 0xB5 -> orr A L
        | 0xB6 -> orr A (Pointer HL)
        | 0xB7 -> orr A A
        | 0xF6 -> orr A N8
        //XOR
        | 0xA8 -> xor A B
        | 0xA9 -> xor A C
        | 0xAA -> xor A D
        | 0xAB -> xor A E
        | 0xAC -> xor A H
        | 0xAD -> xor A L
        | 0xAE -> xor A (Pointer HL)
        | 0xAF -> xor A A
        | 0xEE -> xor A N8
        //ADC
        | 0x88 -> adc A B
        | 0x89 -> adc A C
        | 0x8A -> adc A D
        | 0x8B -> adc A E
        | 0x8C -> adc A H
        | 0x8D -> adc A L
        | 0x8E -> adc A (Pointer HL)
        | 0x8F -> adc A A
        | 0xCE -> adc A N8
        //SBC
        | 0x98 -> sbc A B
        | 0x99 -> sbc A C
        | 0x9A -> sbc A D
        | 0x9B -> sbc A E
        | 0x9C -> sbc A H
        | 0x9D -> sbc A L
        | 0x9E -> sbc A (Pointer HL)
        | 0x9F -> sbc A A
        | 0xDE -> sbc A N8
        //CP
        | 0xB8 -> cp A B
        | 0xB9 -> cp A C
        | 0xBA -> cp A D
        | 0xBB -> cp A E
        | 0xBC -> cp A H
        | 0xBD -> cp A L
        | 0xBE -> cp A (Pointer HL)
        | 0xBF -> cp A A
        | 0xFE -> cp A N8
        | _ ->  None
    | _ -> None