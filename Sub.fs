module Sub
open Registers
open MemoryBus
open Instructions
open Flags




let isCarry7to8 (x : byte) (y : byte) =
    let res = (x - y) >>> 8
    res > 0uy

let isHalfCarry3to4 (x :byte) (y:byte) =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    ((maskedX - maskedY) >>> 4) > 0uy

let isZero (n : uint) =
    n = 0u

let setMin (n : byte) carry : byte =
    match carry with
    | true -> 0uy
    | false -> n

let sub8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x - y
        let carry, half = isCarry7to8 x y, isHalfCarry3to4 x y
        let z = isZero (uint res)
        let resMin = setMin res carry
        let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
        let updatedRegisters = regs |> Map.add a (B8 resMin)
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let filterSub (r1,(r2 : Resource)) reg =
    match r2 with
    | Vregister HL -> None
    | Register n -> sub8bitRegisters A n reg
    | _ -> None