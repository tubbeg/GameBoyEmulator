module Registers

type BitData =
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


let translateVirtualReg reg =
    match reg with
    | AF -> A,F
    | BC -> B,C
    | DE -> D,E
    | HL -> H,L


type RegisterMap = Map<GameBoyRegister, BitData>

let registers : RegisterMap =
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

let shortToByte (s : uint16) =
    let high = byte (s >>> 8)
    let low = byte (s&&&0x00FFus)
    high,low

let updateVirtualReg (data : uint16) reg (regs : RegisterMap) =
    printf "Updating %A with data %A "  reg data
    let a,b = translateVirtualReg reg
    let high,low = shortToByte data
    printf "High %A Low %A" high low
    let update = regs |> Map.add a (B8 high) |> Map.add b (B8 low)
    update


let bytesToVirtual high low  =
    let orBit (l : byte) (h : uint16) : uint16 =
        let a : uint16 = uint16 h
        let b = uint16 l
        a^^^b //order is irrelevant
    let bitShift (n : byte) : uint16 =
        let a : uint16 = uint16 n
        a <<< 8
    match high,low with
    | Some(B8 h),Some(B8 l) -> Some (bitShift h |> orBit l)
    | _ -> None


let regsToVirtual high low (regs : RegisterMap) : uint16 option =
    let h = regs |> Map.tryFind high
    let l = regs |> Map.tryFind low
    bytesToVirtual h l


let getPCregister (registers : Map<GameBoyRegister,BitData>) =
    registers |> Map.tryFind PC 