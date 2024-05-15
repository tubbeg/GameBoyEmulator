module Registers

type Register =
    B16 of uint16 | B8 of byte

type GameBoyRegister =
    | A
    | B 
    | C
    | D
    | E
    | F
    | H
    | L 
    | PC
    | SP

let defaultByte = 0uy
let defaultShort = 0us

type VirtualRegister =
    | AF 
    | BC
    | DE 
    | HL

let registers : Map<GameBoyRegister, Register> =
    Map.empty
    |> Map.add A (B8 defaultByte)
    |> Map.add B (B8 defaultByte)
    |> Map.add C (B8 defaultByte)
    |> Map.add D (B8 defaultByte)
    |> Map.add E (B8 defaultByte)
    |> Map.add F (B8 defaultByte)
    |> Map.add H (B8 defaultByte)
    |> Map.add L (B8 defaultByte)
    |> Map.add SP (B16 defaultShort)
    |> Map.add PC (B16 defaultShort)

let regsToVirtual high low  =
    let orBit (l : byte) (h : uint16) : uint16 =
        let a : uint16 = uint16 h
        let b = uint16 l
        a^^^b //order is irrelevant
    let bitShift (n : byte) : uint16 =
        let a : uint16 = uint16 n
        a <<< 8
    match high,low with
    | B8 h,B8 l -> Some (bitShift h |> orBit l)
    | _ -> None


let getPCregister (registers : Map<GameBoyRegister,Register>) =
    registers |> Map.tryFind PC 