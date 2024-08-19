module Instruction
open Register
open Memory
open ALU
open Opcodes
open Fetch
open Utility
open Arithmetic


let addShortUpdateStatus h l fetch (parse : OperandParse) =
    match addShorts h l fetch.registers with
    | Some (sh,s) ->
        updateRegisterOption parse.operand1 (Sh sh) fetch.registers |>
        updateStatus s
    | _ -> None

let addNegativeToSP sh n (parse : OperandParse) f =
    let toPositiveNumber = (n * (-1)) |> intToByte
    match toPositiveNumber with
    | Some nr ->
        match subShorts sh ((N 0, N 0), nr) (f.registers) with
        | Some (sh,s) ->
            updateRegisterOption (Some SP) (Sh sh) f.registers |>
            updateStatus s
        | _ -> None
    | _ -> None
    
let addByteToSP sh n (parse : OperandParse) f =
    match addShorts sh ((N 0, N 0), n) (f.registers) with
    | Some (sh,s) ->
        updateRegisterOption (Some SP) (Sh sh) f.registers |>
        updateStatus s
    | _ -> None

let addSignedByteToSP (sh : Short) (e8 : SignedByte) f (parse : OperandParse) =
    match (e8 |> signedByteToInt) with
    | n when (n < 0) -> addNegativeToSP sh n parse f
    | p ->
        let msg  =  "Check that the flags are correct here.
        Overflow from bit 7 according to docs. But SP is still a short!
        Overflow is possible"
        eprintfn "%A" msg
        match (p |> intToByte) with
        | Some positive ->
            addByteToSP sh positive parse f
        | _ -> None

let addInstruction (parse : OperandParse) (fetch : Fetch) =
    match parse.operand1,fetch.operand1,fetch.operand2 with
    | Some A,Some (By h), Some (By l) -> addBytesToAcc h l fetch.registers
    | Some SP,Some (Sh h), Some (Sb d) -> addSignedByteToSP h d fetch parse
    | Some (HL | AF | BC | DE), Some (Sh h), Some (Sh l) ->
        addShortUpdateStatus h l fetch parse

    | _ -> None

let subInstruction (fetch : Fetch) m : Registers option  =
    match fetch.operand1,fetch.operand2 with
    | Some (By h), Some (By l) -> subBytesToAcc h l fetch.registers
    | _ -> None