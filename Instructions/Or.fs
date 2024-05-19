module Or
open Registers
open MemoryBus
open Instructions
open Flags



let setMinMax (n : byte)  =
    match (n < 0uy), (n > 255uy) with
    | true,false -> 0uy
    | false, true -> 255uy
    | _ -> n

let orHLpointer   (regs : RegisterMap) (mem : MemoryBus) r1  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    let pc = regs |> Map.tryFind PC
    printf "PC is: %A" pc
    printf "Memory at PC is: " 
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res =   
                match instr with
                | OR(_,_) -> b ||| dataHL
                | _ -> b ^^^ dataHL
            let f = {Zero=(res = 0uy);Sub=false;HalfCarry=false;Carry=false}
            let ur = regs |> Map.add A (B8 (setMinMax res))
            Some(ur,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let or8bitRegisters  a b (regs : RegisterMap) r1 :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res =   
            match instr with
            | OR(_,_) -> x ||| y
            | _ -> x ^^^ y
        let f = {Zero=(res = 0uy);Sub=false;HalfCarry=false;Carry=false}
        let updatedRegisters = regs |> Map.add a (B8 (setMinMax res))
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let orFromMem (regs : RegisterMap) (memory : MemoryBus) (instr : Instruction) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            printf "N8 is %A" n
            printf "REG is %A" i
            let res =   
                match instr with
                | OR(_,_) -> i ||| n
                | _ -> i ^^^ n
            let f = {Zero=(res = 0uy);Sub=false;HalfCarry=false;Carry=false}
            let updatedRegs =
                regs |> Map.add A (B8 (setMinMax res )) // update result in registers
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedRegs,f)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None

let filterOrXor (r1,(r2 : Resource)) reg mem (i : Instruction) =
    match r2 with
    | Pointer(Vregister HL) -> orHLpointer reg mem i
    | Number N8 -> orFromMem reg mem i
    | Register n -> or8bitRegisters A n reg i
    | _ -> None