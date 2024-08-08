module ALU
open Register
open Opcodes

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

let setCarry i ins =
    match ins with
    | AND | XOR | OR -> false
    | _ -> (i > 255) || (i < 0)

let setZero i =
    i = 0

let setHalfCarry (a : Byte) (b : Byte) ins =
    match ins with
    | AND -> true
    | XOR | OR -> false
    | SUB | SBC ->
        let result = a.lowNibble - b.lowNibble
        (result > 0x0F) || (result < 0)
    | _ ->
        let result = a.lowNibble + b.lowNibble
        (result > 0x0F) || (result < 0)

let setFlags a b result ins =
    {
        zero=setZero result;
        sub=(ins=SUB || ins=SBC);
        carry=setCarry result ins;
        half=setHalfCarry a b ins
    }

let boolToBit b =
    match b with
    | true -> 1
    | _ -> 0

(* 
a -> b -> status -> opcode -> (y,status)

DEC and INC can be implemented as an SUB/ADD
LD can be implemented as OR followed by AND
*)
let alu (input : ALUinput) : ALUoutput option =
    //adc and sbc need the carry bit from status
    let a,b,ins,c =
        input.a, input.b,input.i,(boolToBit input.s.carry)
    match ins with
    | ADD ->
        let result = a.byteValue + b.byteValue
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | ADC ->
        let result = a.byteValue + b.byteValue + c
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | SUB ->
        let result = a.byteValue - b.byteValue
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | SBC ->
        let result = a.byteValue - b.byteValue - c
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | OR ->
        let result = a.byteValue ||| b.byteValue
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | XOR ->
        let result = a.byteValue ^^^ b.byteValue
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | AND ->
        let result = a.byteValue &&& b.byteValue 
        let flags = setFlags a b result ins
        Some {y=Byte result;s=flags}
    | _ -> error ins