module Compare
open Registers
open MemoryBus
open Instructions
open Flags




let isBorrowBit4 (x :byte) (y:byte) =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    (maskedX > maskedY) //borrow from bit 3 or bit 4?


let isZero (n : uint) =
    n = 0u

let setMin (n : byte) carry : byte =
    match carry, (n > 0uy) with
    | true,_ -> 0uy
    | _,false -> 0uy
    | false,true -> n


let cpHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = b - dataHL
            let carry = dataHL > b
            let half = isBorrowBit4 dataHL b
            let z = isZero (uint res)
            let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
            Some(regs,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let cp8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x - y
        let carry, half = (y > x), isBorrowBit4 y x
        let z = isZero (uint res)
        let resMin = setMin res carry
        let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
        Some(regs,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let cpFromMem (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            let res = i - n
            let carry = (n > i)
            let half = isBorrowBit4 n i
            let z = isZero (uint res)
            let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
            let updatedRegs =
                regs
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedRegs,f)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None

let filterCompare (r1,(r2 : Resource)) reg mem =
    match r2 with
    | Pointer(Vregister HL) -> cpHLpointer reg mem
    | Number N8 -> cpFromMem reg mem
    | Register n -> cp8bitRegisters A n reg
    | _ -> None