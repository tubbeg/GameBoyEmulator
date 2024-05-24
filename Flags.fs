module Flags
open Registers


type Flags = {Zero:bool;Sub:bool;HalfCarry:bool;Carry:bool}


let getCarryBit b =
    match b with
    | Some(B8 byte) -> (0x10uy&&&byte) >>> 4
    | _ ->
        printf "Incorrect flag byte!"
        0uy


let isCarry (byte : BitData option) =
    (getCarryBit byte > 0uy)

let isHalfCarry (byte : BitData option) =
    match byte with
    | Some(B8 b) ->
        let hcb = (0x20uy&&&b)
        (hcb > 0uy)
    | _ ->
        printf "Incorrect flag byte!"
        false

let isSub (byte : BitData option) =
    match byte with
    | Some(B8 b) ->
        let hcb = (0x40uy&&&b)
        (hcb > 0uy)
    | _ ->
        printf "Incorrect flag byte!"
        false

let isZero (byte : BitData option) =
    match byte with
    | Some(B8 b) ->
        let hcb = (0x80uy&&&b)
        (hcb > 0uy)
    | _ ->
        printf "Incorrect flag byte!"
        false


let convertRegToFlagRec byte : Flags =
    {Zero=isZero byte; Sub=isSub byte;
    Carry=isCarry byte; HalfCarry=isHalfCarry byte}