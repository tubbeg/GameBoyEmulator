module Arithmetic
open Utility
open Register
open ALU
open Fetch
open Opcodes

let addNibbles a b status =
    alu {a=a;b=b;s=status;i=Operation.ADD}

let adcNibbles a b status =
    alu {a=a;b=b;s=status;i=Operation.ADC}

let subNibbles a b status =
    alu {a=a;b=b;s=status;i=Operation.SUB}

let sbcNibbles a b status =
    alu {a=a;b=b;s=status;i=Operation.SBC}

let subBytes (a : Byte) (b : Byte) (registers : Registers) =
    let (ha,la) = a
    let (hb,lb) = b
    let s = registers.F |> processorStatusToALUstatus
    match subNibbles la lb s with
    | Some subtraction ->
        match sbcNibbles ha hb subtraction.s with
        | Some sbctraction ->
            let byte = sbctraction.y,subtraction.y
            let status =
                aluStatusToProcessorStatus subtraction sbctraction
            Some (byte,status)
        | _ -> None
    | _ -> None

let addBytes (a : Byte) (b : Byte) (registers : Registers) =
    let (ha,la) = a
    let (hb,lb) = b
    let s = registers.F |> processorStatusToALUstatus
    match addNibbles la lb s with
    | Some addition ->
        match adcNibbles ha hb addition.s with
        | Some adcition ->
            let byte = adcition.y,addition.y
            let status =
                aluStatusToProcessorStatus addition adcition
            Some (byte,status)
        | _ -> None
    | _ -> None


let adcBytes (a : Byte) (b : Byte) (registers : Registers) =
    let (ha,la) = a
    let (hb,lb) = b
    let s = registers.F |> processorStatusToALUstatus
    match adcNibbles la lb s with
    | Some adcition ->
        match adcNibbles ha hb adcition.s with
        | Some adcition2 ->
            let byte = adcition2.y,adcition.y
            let status =
                aluStatusToProcessorStatus adcition adcition2
            Some (byte,status)
        | _ -> None
    | _ -> None


let sbcBytes (a : Byte) (b : Byte) (registers : Registers) =
    let (ha,la) = a
    let (hb,lb) = b
    let s = registers.F |> processorStatusToALUstatus
    match sbcNibbles la lb s with
    | Some sbcition ->
        match sbcNibbles ha hb sbcition.s with
        | Some sbcition2 ->
            let byte = sbcition2.y,sbcition.y
            let status =
                aluStatusToProcessorStatus sbcition sbcition2
            Some (byte,status)
        | _ -> None
    | _ -> None

let addShorts (a : Short) (b : Short) (registers : Registers)  =
    let (ha,la) = a
    let (hb,lb) = b
    match addBytes la lb registers with
    | Some( lowByte, status1) ->
        match (updateStatus status1 (Some registers)) with
        | Some s ->
            match adcBytes ha hb s  with
            | Some (highByte, s2) ->
                let short = highByte,lowByte
                Some (short,s2)
            | _ -> None
        | _ -> None
    | _ -> None

let subShorts (a : Short) (b : Short) (registers : Registers)  =
    let (ha,la) = a
    let (hb,lb) = b
    match subBytes la lb registers with
    | Some( lowByte, status1) ->
        match (updateStatus status1 (Some registers)) with
        | Some s ->
            match sbcBytes ha hb s  with
            | Some (highByte, s2) ->
                let short = highByte,lowByte
                Some (short,s2)
            | _ -> None
        | _ -> None
    | _ -> None


let addBytesToAcc a b registers : Registers option  = 
    match addBytes a b registers with
    | Some (y,s) ->
        updateRegister A (By y) (Some registers) |> updateStatus s 
    | _ -> None 

let subBytesToAcc a b registers : Registers option  = 
    match subBytes a b registers with
    | Some (y,s) ->
        updateRegister A (By y) (Some registers) |> updateStatus s 
    | _ -> None 

let adcBytesToAcc a b registers : Registers option  = 
    match adcBytes a b registers with
    | Some (y,s) ->
        updateRegister A (By y) (Some registers) |> updateStatus s 
    | _ -> None 

let sbcBytesToAcc a b registers : Registers option  = 
    match sbcBytes a b registers with
    | Some (y,s) ->
        updateRegister A (By y) (Some registers) |> updateStatus s 
    | _ -> None 