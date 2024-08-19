module ALU

type Zero = ZERO of bool
type Sub = SUB of bool
type Carry = CARRY of bool
type Status = Zero * Sub * Carry

type Alpha =
    | A
    | B
    | C
    | D
    | E
    | F

let alphaToNumber a =
    match a with
    | A -> 10
    | B -> 11
    | C -> 12
    | D -> 13
    | E -> 14
    | F -> 15

let numberToAlpha a =
    match a with
    | 10 -> Some A
    | 11 -> Some B
    | 12 -> Some C
    | 13 -> Some D
    | 14 -> Some E
    | 15 -> Some F
    | _ -> None

let negativeBase16Mod n =
    match (n % 16) with
    | 0 -> 0
    | i -> i + 16

let bit4mod value =
    match value with
    | n when (n > 0) -> n % 16
    | n ->  negativeBase16Mod n

type Hex =
    | N of int
    | A of Alpha

type Nibble = Hex
type Byte = Hex * Hex

let nibbleToInt n =
    match n with
    | N i -> i 
    | A j ->  alphaToNumber j

let intToNibble int =
    let nr = bit4mod int
    match nr with
    | j when (j > 9 && j < 16) ->
        match numberToAlpha j with
        | Some a -> a |> A |> Some
        | _ -> None
    | b -> b |> N |> Some

type Operation = 
    | ADD
    | SUB
    | ADC
    | SBC
    | XOR
    | AND
    | OR

type ALUinput =
    {
        a:Nibble;
        b:Nibble;
        s:Status;
        i:Operation;
    }

type ALUoutput =
    {
        y:Nibble;
        s:Status;
    }

let isCarry i ins =
    match ins with
    | AND | XOR | OR -> false
    | _ -> (i > 15) || (i < 0)

let isZero i =
    i = 0

let isSub ins =
    ins=SUB || ins=SBC

let boolToBit b =
    match b with
    | true -> 1
    | _ -> 0

let determineStatus result ins  =
    let z = (result = 0) |> ZERO
    let c = isCarry result ins |> CARRY
    let s  = isSub ins |> Sub.SUB
    z,s,c

let getCarryStatus (s : Status) =
    match s with
    | _,_,(CARRY b) -> b

let aluAdd a b i =
    let result = a + b
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None

let aluSub a b i =
    let result = a - b
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None

let aluAdc a b i s =
    let c = s |> getCarryStatus |> boolToBit
    let result = a + b + c
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None

let aluSbc a b i s =
    let c = s |> getCarryStatus |> boolToBit
    let result = a - b - c
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None

let aluOr a b i =
    let result = a ||| b
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None


let aluXor a b i =
    let result = a ^^^ b
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None

let aluAnd a b i =
    let result = a &&& b
    let flags = determineStatus result i
    match intToNibble result with
    | Some n -> Some {y=n;s=flags}
    | _ -> None


(* 
a -> b -> status -> opcode -> (y,status)

DEC and INC can be implemented as an SUB/ADD
LD can be implemented as OR followed by AND

CP can be performed as SUB, but don't store
result in accumulator


The Sharp SM83 is quite similar to the Z80,but
not identical. The Z80 actually uses a 4-bit ALU!

Which is why this implementation also uses a 
4 bit alu.

*)
let alu (input : ALUinput) : ALUoutput option =
    //adc and sbc need the carry bit from status
    let a,b =
        input.a |> nibbleToInt, input.b |> nibbleToInt
    match input.i with
    | ADD -> aluAdd a b input.i
    | ADC -> aluAdc a b input.i input.s
    | SUB -> aluSub a b input.i
    | SBC -> aluAdc a b input.i input.s
    | OR -> aluOr a b input.i
    | XOR -> aluXor a b input.i
    | AND -> aluAnd a b input.i