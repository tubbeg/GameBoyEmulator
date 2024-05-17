module Flags
open Registers


type Flags = {Zero:bool;Sub:bool;HalfCarry:bool;Carry:bool}


let getCarryBit b =
    match b with
    | Some(B8 byte) -> (0x10uy&&&byte) >>> 4
    | _ ->
        printf "Incorrect flag byte!"
        0uy