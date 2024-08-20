module Opcodes
open Register
open Fable.Core
open ALU



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
    | LDH
    | RLCA
    | RLA
    | STOP
    | RL
    | JR
    | DAA
    | SCF
    | RET
    | JP
    | DI
    | RST
    | ILLEGAL
    | RETI
    | EI
    | RRC
    | RR
    | SLA
    | SRA
    | SRL
    | BIT



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
    | SPC // 0xF8
    | AF
    | BC
    | DE 
    | HL
    | HLI
    | HLD
    | N8
    | E8
    | A8
    | A16
    | N16
    | Pointer of Operand
    | PC //not actually an operand
    | Constant of Byte
    | NotZero 
    | Zero 
    | NotCarry 
    | Carry
    | Bit of int

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


let nibbleToHexString h =
    match h with
    | (Hex.A l) ->
        match l with
        | Alpha.A -> "A"
        | Alpha.B -> "B"
        | Alpha.C -> "C"
        | Alpha.D -> "D"
        | Alpha.E -> "E"
        | Alpha.F -> "F"
    | (N n) -> 
        match n with
        | 1 -> "1"
        | 2 -> "2"
        | 3 -> "3"
        | 4 -> "4"
        | 5 -> "5"
        | 6 -> "6"
        | 7 -> "7"
        | 8 -> "8"
        | 9 -> "9"
        | _ -> "0"

let toHexString byte =
    let a,b = byte
    let high,low = nibbleToHexString a, nibbleToHexString b
    ("0x" + high + low)

[<Import("*","./Opcodes.json")>]
let opcodes : string = jsNative

[<Emit("$1[\"unprefixed\"][$0][\"mnemonic\"]")>]
let getMnemonic hex opcodes : string = jsNative
[<Emit("$1[\"cbprefixed\"][$0][\"mnemonic\"]")>]
let getMnemonicPrefixed hex opcodes : string = jsNative

[<Emit("$1[\"unprefixed\"][$0][\"operands\"][0]")>]
let getFirstOperand hex opcodes : string = jsNative

[<Emit("$1[\"unprefixed\"][$0][\"operands\"][1]")>]
let getSecondOperand hex opcodes : string = jsNative

[<Emit("$1[\"cbprefixed\"][$0][\"operands\"][0]")>]
let getFirstOperandPrefix hex opcodes : string = jsNative

[<Emit("$1[\"cbprefixed\"][$0][\"operands\"][1]")>]
let getSecondOperandPrefix hex opcodes : string = jsNative

[<Emit("$0[\"name\"]")>]
let getOperandName operand : string option  = jsNative
[<Emit("$0[\"immediate\"]")>]
let getOperandImmediate operand : bool = jsNative
[<Emit("$0[\"increment\"]")>]
let getOperandIncrement operand : bool option = jsNative
[<Emit("$0[\"decrement\"]")>]
let getOperandDecrement operand : bool option = jsNative


let parseOpJson o ins hex : Operand option = 
    printfn "%A %A" ins hex
    let n = getOperandName o
    let d = getOperandDecrement o
    let i = getOperandIncrement o
    match n with
    | None ->   
        eprintfn "Operand %A not found! Instruction: %A" n ins
        None 
    | Some name ->
        match name,i,d with
        | "A",_,_ -> A |> Some
        | "B",_,_ -> B |> Some
        | "C",_,_ ->
            match ins with
            | CALL | JR | RET -> Carry |> Some
            | _ -> C |> Some
        | "D",_,_ -> D |> Some
        | "E",_,_ -> E |> Some
        | "F",_,_ -> F |> Some
        | "H",_,_ -> H |> Some
        | "L",_,_ -> L |> Some
        | "SP",_,_ -> SP |> Some
        | "n8",_,_ -> N8 |> Some
        | "n16",_,_ -> N16 |> Some
        | "a16",_,_-> A16 |> Some
        | "a8",_,_ -> A8 |> Some
        | "e8",_,_ -> E8 |> Some
        | "AF",_,_ -> AF |> Some
        | "BC",_,_ -> BC |> Some
        | "DE",_,_ -> DE |> Some
        | "HL",None,None -> HL |> Some
        | "HL",Some true,_ -> HLI |> Some
        | "HL",_,Some true -> HLD |> Some
        | "$00",_,_ -> (Constant (N 0, N 0)) |> Some
        | "$08",_,_  -> (Constant (N 0, N 8)) |> Some
        | "$10",_,_ -> (Constant (N 1, N 0)) |> Some
        | "$18",_,_ -> (Constant (N 1, N 8)) |> Some
        | "$20",_,_ -> (Constant (N 2, N 0)) |> Some
        | "$28",_,_ -> (Constant (N 2, N 8)) |> Some
        | "$30",_,_ -> (Constant (N 3, N 0)) |> Some
        | "$38",_,_ -> (Constant (N 3, N 8)) |> Some
        | "NZ",_,_ -> NotZero |> Some
        | "Z",_,_ -> Zero |> Some
        | "NC",_,_ -> NotCarry |> Some
        | "0",_,_ -> (Bit 0) |> Some
        | "1",_,_ -> (Bit 1) |> Some
        | "2",_,_ -> (Bit 2) |> Some
        | "3",_,_ -> (Bit 3) |> Some
        | "4",_,_ -> (Bit 4) |> Some
        | "5",_,_ -> (Bit 5) |> Some
        | "6",_,_ -> (Bit 6) |> Some
        | "7",_,_ -> (Bit 7) |> Some
        | _ ->
            eprintfn "Error. Operand %A not found" name
            eprintfn "Instruction %A" ins
            None


let addPointer o parse : Operand option = 
    let isPointer = getOperandImmediate o
    match isPointer,parse with
    | false,Some p ->
        (Pointer p) |> Some
    | true, op -> op
    | _ -> None

let parseOperand hex ins (o : string) =
    parseOpJson o ins hex |> addPointer o

let stringToInstruction s =
    match s with
    | "ADD" -> ADD |> Some
    | "SUB" -> SUB |> Some
    | "ADC" -> ADC |> Some
    | "SBC" -> SBC |> Some
    | "OR" -> OR |> Some
    | "AND" -> AND |> Some
    | "XOR" -> XOR |> Some
    | "LD" -> LD |> Some
    | "NOP" -> NOP |> Some
    | "INC" -> INC |> Some
    | "DEC" -> DEC |>  Some
    | "RLCA" -> RLCA |> Some
    | "RRCA" -> RRCA |> Some
    | "STOP" -> STOP |> Some
    | "RLA" -> RLA |> Some
    | "JR" -> JR |> Some
    | "RRA" -> RRA |> Some
    | "DAA" -> DAA |> Some
    | "CPL" -> CPL |> Some
    | "SCF" -> SCF |> Some
    | "CCF" -> CCF |> Some
    | "HALT" -> HALT |> Some
    | "CP" -> CP |> Some
    | "RET" -> RET |> Some
    | "POP" -> POP |> Some
    | "JP" -> JP |> Some
    | "CALL" -> CALL |> Some
    | "PUSH" -> PUSH |> Some
    | "RST" -> RST |> Some
    | "PREFIX" -> PREFIX |> Some
    | "ILLEGAL_D3" -> ILLEGAL |> Some
    | "ILLEGAL_DB" -> ILLEGAL |> Some
    | "ILLEGAL_DD" -> ILLEGAL |> Some
    | "ILLEGAL_E3" -> ILLEGAL |> Some
    | "ILLEGAL_E4" -> ILLEGAL |> Some
    | "ILLEGAL_EB" -> ILLEGAL |> Some
    | "ILLEGAL_EC" -> ILLEGAL |> Some
    | "ILLEGAL_ED" -> ILLEGAL |> Some
    | "ILLEGAL_F4" -> ILLEGAL |> Some
    | "ILLEGAL_FC" -> ILLEGAL |> Some
    | "ILLEGAL_FD" -> ILLEGAL |> Some
    | "LDH" -> LDH |> Some
    | "RETI" -> RETI |> Some
    | "DI" -> DI |> Some
    | "EI" -> EI |> Some
    | "RLC" -> RLC |> Some
    | "RRC" -> RRC |> Some
    | "RL" -> RL |> Some
    | "RR" -> RR |> Some
    | "SLA" -> SLA |> Some
    | "SRA" -> SRA |> Some
    | "SWAP" -> SWAP |> Some
    | "SRL" -> SRL |> Some
    | "BIT" -> BIT |> Some
    | "RES" -> RES |> Some
    | "SET" -> SET |> Some
    | _ ->
        eprintfn "Failed to parse string instruction: %A" s
        None

let jsonTo2operands (hex : string) opcodes ins prefix =
    let operands  =
        match prefix with
        | false ->
            getFirstOperand hex opcodes |> parseOperand hex ins,
            getSecondOperand hex opcodes |> parseOperand hex ins
        | _ ->
            getFirstOperandPrefix hex opcodes |> parseOperand hex ins,
            getSecondOperandPrefix hex opcodes |> parseOperand hex ins

    match operands with
    | (Some n1,Some n2) -> opRecord2operands ins n1 n2
    | _ ->
        eprintfn "Failed to parse operands %A" hex
        None

let jsonTo1operand (hex : string) opcodes ins prefix =
    let operand  =
        match prefix with
        | false -> getFirstOperand hex opcodes |> parseOperand hex ins
        | _ -> getFirstOperandPrefix hex opcodes |> parseOperand hex ins
    match operand with
    | Some o1 -> opRecord1operand ins o1
    | _ ->
        eprintfn "Failed to parse operand %A" hex
        None

let handleJr hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0x18) ->
        jsonTo1operand nr opcodes i false
    | _ -> 
        jsonTo2operands nr opcodes i false

let handleCall hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0xCD) ->
        jsonTo1operand nr opcodes i false
    | _ -> 
        jsonTo2operands nr opcodes i false

let handleJp hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0xC3) ->
        jsonTo1operand nr opcodes i false
    | n when (n = 0xE9) ->
        jsonTo1operand nr opcodes i false
    | _ -> 
        jsonTo2operands nr opcodes i false

let handleRet hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0xC9) ->
        opRecordNooperand i
    | _ -> 
        jsonTo1operand nr opcodes i false

let jsonToRecord hex isPrefix  =  
    let nr = hex |> toHexString
    let instruction =
        match isPrefix with
        | true -> getMnemonic nr opcodes |> stringToInstruction
        | _ -> getMnemonicPrefixed nr opcodes |> stringToInstruction
    match instruction with
    | Some i ->
        match i with
        | ADD | SUB | SBC | ADC | CP | OR | XOR | AND  | LD | LDH ->
            jsonTo2operands nr opcodes i false
        | BIT | RES | SET ->
            jsonTo2operands nr opcodes i true
        | INC | DEC | POP | RST | PUSH | STOP  ->
            jsonTo1operand nr opcodes i false
        | RLC | RL | RR | SLA | SWAP | SRL | SRA | RRC ->
            jsonTo1operand nr opcodes i true
        | NOP | CPL | CCF | RETI | PREFIX | DI | EI | ILLEGAL | HALT | RLCA
        | RRCA | RRA | SCF | DAA | RLA -> opRecordNooperand i 
        | JR -> handleJr hex nr i
        | RET -> handleRet hex nr i
        | JP -> handleJp hex nr i
        | CALL -> handleCall hex nr i
    | _ -> None
