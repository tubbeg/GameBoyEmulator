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
let getOperandName operand : string  = jsNative
[<Emit("$0[\"immediate\"]")>]
let getOperandImmediate operand : bool = jsNative
[<Emit("$0[\"increment\"]")>]
let getOperandIncrement operand : bool option = jsNative
[<Emit("$0[\"decrement\"]")>]
let getOperandDecrement operand : bool option = jsNative


let parseOpJson o ins : Operand option = 
    let name = getOperandName o
    let d = getOperandDecrement o
    let i = getOperandIncrement o
    match name,i,d with
    | "A",_,_ -> A |> Some
    | "B",_,_ -> B |> Some
    | "C",_,_ -> C |> Some
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

let parseOperand  ins (o : string) =
    parseOpJson o ins |> addPointer o

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
    | _ ->
        eprintfn "Failed to parse string instruction: %A" s
        None

let jsonTo2operands (hex : string) opcodes ins =
    let operands  =
        getFirstOperand hex opcodes |> parseOperand ins,
        getSecondOperand hex opcodes |> parseOperand ins
    match operands with
    | (Some n1,Some n2) -> opRecord2operands ins n1 n2
    | _ ->
        eprintfn "Failed to parse operands %A" hex
        None

let jsonTo1operand (hex : string) opcodes ins =
    let operand  = getFirstOperand hex opcodes |> parseOperand ins
    match operand with
    | Some o1 -> opRecord1operand ins o1
    | _ ->
        eprintfn "Failed to parse operand %A" hex
        None

let handleJr hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0x18) ->
        jsonTo1operand nr opcodes i
    | _ -> 
        jsonTo2operands nr opcodes i

let handleRet hex nr i = 
    //JR can have one or two operands
    match (byteToInt hex) with
    | n when (n = 0xC9) ->
        jsonTo1operand nr opcodes i
    | _ -> 
        opRecordNooperand i

let jsonToRecord hex  =  
    let nr = hex |> toHexString
    let instruction = getMnemonic nr opcodes |> stringToInstruction
    match instruction with
    | Some i ->
        match i with
        | ADD | SUB | SBC | ADC | CP | OR | XOR | AND  | LD | LDH ->
            jsonTo2operands nr opcodes i
        | INC | DEC | POP | PUSH | STOP | RL | RLC ->
            jsonTo1operand nr opcodes i
        | NOP | CPL | CCF | HALT | RLCA | RRCA | RRA | SCF | DAA | RLA ->
            opRecordNooperand i 
        | JR -> handleJr hex nr i
        | RET -> handleRet hex nr i
        | _ ->
            eprintfn "Missing instruction: %A" instruction
            None
    | _ -> None