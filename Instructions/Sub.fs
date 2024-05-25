module Sub
open Registers
open MemoryBus
open Instructions
open Flags
open Fetch





let setMinMax (n : byte) : byte =
    match (n > 255uy), (n < 0uy) with
    | true,_ ->
        printfn "Overflow. Setting max."
        255uy
    | _,true ->
        printfn "Overlow. Setting min."
        0uy
    | false,false -> n


let sub (r1,(r2 : Resource)) reg mem =
    let d1,regs2,adress = fetchResource r1 reg mem
    let d2, regs3,_ = fetchResource r2 regs2 mem
    match d1,d2 with
    | Some(B8 data1), Some(B8 data2) ->
        let result =  (data1 - data2)
        let resultNibble = (data1&&&0x0Fuy) - (data2&&&0x0Fuy)
        updateResource r1 (setFlags result resultNibble true  regs3) mem (B8 (setMinMax result)) adress
    | _ -> None