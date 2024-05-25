module Flags
open Registers


//helper function. Only to be used for debugging and development. Not for prod
let printFlags (rm : RegisterMap option) =
    match rm with
    | None -> printfn "No regs detected"
    | Some registers ->
        let flags = registers |> Map.tryFind F
        match flags with
        | Some (B8 f) ->
            let zero = (f&&&0x80uy) > 0uy
            let sub = (f&&&0x40uy) > 0uy
            let half = (f&&&0x20uy) > 0uy
            let carry = (f&&&0x10uy) > 0uy
            printfn "Zero: %A Sub %A Half %A Carry %A" zero sub half carry
        | _ -> printfn "No flags detected"


let isCarry result = 
    (result > 255uy) || (result < 0uy)

let halfCarry (result : byte) =
    (result > 15uy) || (result < 0uy)



let setFlags resCarry resNibble sub (rm : RegisterMap option) =
    match rm with
    | (Some registers) ->
        let isZero = (resCarry = 0uy)
        let isCarry,halfCarry = isCarry resCarry, halfCarry resNibble 
        let sub = match sub with | true -> 0x40uy | false -> 0x00uy
        let addZero = match isZero with | true -> (sub + 0x80uy) | false -> sub
        let addCarry = match isCarry with | true -> (addZero + 0x10uy) | false -> addZero
        let flags = match halfCarry with true | true -> (addCarry + 0x20uy) | false -> addCarry
        registers |> Map.add F (B8 flags) |> Some
    | _ -> None