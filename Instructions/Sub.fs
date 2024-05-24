module Sub
open Registers
open MemoryBus
open Instructions
open Flags
open Fetch



let isCarry7to8 (x : byte) (y : byte) =
    let res = (x - y) >>> 8
    res > 0uy

let isHalfCarry3to4 (x :byte) (y:byte) =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    ((maskedX - maskedY) >>> 4) > 0uy

let setMinMax (n : byte) : byte =
    match (n > 255uy), (n < 0uy) with
    | true,_ -> 255uy
    | _,true -> 0uy
    | false,false -> n


//Setting the flags can be a little bit problematic
//since it's different depending on the instruction; It's
//harder to set flags using a generic function
let setFlags a b (rm : RegisterMap option) =
    match rm with
    | (Some registers) ->
        let res = a - b
        let isZero = (res = 0uy)
        let isCarry = isCarry7to8 a b
        let halfCarry = isHalfCarry3to4 a b
        let reset = 0x40uy //sub is set to true
        let addZero = match isZero with | true -> (reset + 0x80uy) | false -> reset
        let addCarry = match isCarry with | true -> (addZero + 0x10uy) | false -> addZero
        let flags = match halfCarry with true | true -> (addCarry + 0x20uy) | false -> addCarry
        registers |> Map.add F (B8 flags) |> Some
    | _ -> None
        

let filterSub (r1,(r2 : Resource)) reg mem =
    let d1,regs2,adress = fetchResource r1 reg mem
    let d2, regs3,_ = fetchResource r2 regs2 mem
    match d1,d2 with
    | Some(B8 data1), Some(B8 data2) ->
        let result =  (data1 - data2)
        updateResource r1 (setFlags data1 data2 regs3) mem (B8 result) adress
    | _ -> None