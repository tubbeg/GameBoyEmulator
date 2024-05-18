module And
open Registers
open MemoryBus
open Instructions
open Flags


let setMinMax (n : byte)  =
    match (n < 0uy), (n > 255uy) with
    | true,false -> 0uy
    | false, true -> 255uy
    | _ -> n

let andHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = b&&&dataHL
            let f = {Zero=(res = 0uy);Sub=false;HalfCarry=true;Carry=false}
            let ur = regs |> Map.add A (B8 (setMinMax res))
            Some(ur,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let and8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x&&&y
        let f = {Zero=(res = 0uy);Sub=false;HalfCarry=true;Carry=false}
        let updatedRegisters = regs |> Map.add a (B8 (setMinMax res))
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let andFromMem (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            printf "N8 is %A" n
            printf "REG is %A" i
            let res = i&&&n
            let f = {Zero=(res = 0uy);Sub=false;HalfCarry=true;Carry=false}
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

let filterAnd (r1,(r2 : Resource)) reg mem =
    match r2 with
    | Pointer(Vregister HL) -> andHLpointer reg mem
    | Number N8 -> andFromMem reg mem
    | Register n -> and8bitRegisters A n reg
    | _ -> None