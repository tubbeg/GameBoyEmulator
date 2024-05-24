module Load
open Registers
open MemoryBus
open Instructions
open Flags


let loadHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = b - dataHL
            let flags = regs |> Map.tryFind F |> convertRegToFlagRec
            Some(regs,flags)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let load8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x - y
        let flags = regs |> Map.tryFind F |> convertRegToFlagRec
        Some(regs,flags)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let loadFromMem (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            let res = i - n
            let updatedRegs =
                regs
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            
            let flags = regs |> Map.tryFind F |> convertRegToFlagRec
            Some(regs,flags)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None

let filterLoad (r1,(r2 : Resource)) reg mem =
    match r2 with
    | Pointer(Vregister HL) -> loadHLpointer reg mem
    | Number N8 -> loadFromMem reg mem
    | Register n -> load8bitRegisters A n reg
    | _ -> None