module ALU
open Register
open Opcodes

type Status = Byte

type ALUinput =
    {
        a:Byte;
        b:Byte;
        s:Status;
        i:Instruction;
    }

type ALUoutput =
    {
        y:Byte;
        s:Status;
    }

let error msg = 
    eprintfn "Not found: %A" msg
    None

let isCarry i ins =
    match ins with
    | AND | XOR | OR -> false
    | _ -> (i > 255) || (i < 0)

let isZero i =
    i = 0

let isHalfCarry (a : Byte) (b : Byte) ins =
    match ins with
    | AND -> true
    | XOR | OR -> false
    | SUB | SBC ->
        let result = a.lowNibble - b.lowNibble
        (result > 0x0F) || (result < 0)
    | _ ->
        let result = a.lowNibble + b.lowNibble
        (result > 0x0F) || (result < 0)

let isSub ins =
    ins=SUB || ins=SBC

let boolToBit b =
    match b with
    | true -> 1
    | _ -> 0

let calcStatus a b result ins =
    let z = (isZero result |> boolToBit) <<< 3
    let s = (isSub ins |> boolToBit) <<< 2
    let h = (isHalfCarry a b ins |> boolToBit) <<< 1
    let c = (isCarry result ins |> boolToBit)
    (z ||| s ||| h ||| c) |> Byte

(* 
a -> b -> status -> opcode -> (y,status)

DEC and INC can be implemented as an SUB/ADD
LD can be implemented as OR followed by AND

CP can be performed as SUB, but don't store
result in accumulator
*)
let alu (input : ALUinput) : ALUoutput option =
    //adc and sbc need the carry bit from status
    let a,b,ins =
        input.a, input.b,input.i
    match ins,(input.s.getBit 0) with
    | ADD,_ ->
        let result = a.byteValue + b.byteValue
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | ADC,Some c ->
        let result = a.byteValue + b.byteValue + c
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | SUB,_ ->
        let result = a.byteValue - b.byteValue
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | SBC,Some c ->
        let result = a.byteValue - b.byteValue - c
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | OR,_ ->
        let result = a.byteValue ||| b.byteValue
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | XOR,_ ->
        let result = a.byteValue ^^^ b.byteValue
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | AND,_ ->
        let result = a.byteValue &&& b.byteValue 
        let flags = calcStatus a b result ins
        Some {y=Byte result;s=flags}
    | _ -> error ins