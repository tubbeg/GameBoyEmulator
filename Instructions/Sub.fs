module Sub
open Registers
open MemoryBus
open Instructions
open Flags




let isCarry7to8 (x : byte) (y : byte) =
    let res = (x - y) >>> 8
    res > 0uy

let isHalfCarry3to4 (x :byte) (y:byte) =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    ((maskedX - maskedY) >>> 4) > 0uy

let isZero (n : uint) =
    n = 0u

let setMin (n : byte) carry : byte =
    match carry, (n > 0uy) with
    | true,_ -> 0uy
    | _,false -> 0uy
    | false,true -> n


let subHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = b - dataHL
            let carry = isCarry7to8 b dataHL
            let half = isHalfCarry3to4 b dataHL
            let z = isZero (uint res)
            let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
            let ur = regs |> Map.add A (B8 (setMin res carry))
            Some(ur,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let sub8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x - y
        let carry, half = isCarry7to8 x y, isHalfCarry3to4 x y
        let z = isZero (uint res)
        let resMin = setMin res carry
        let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
        let updatedRegisters = regs |> Map.add a (B8 resMin)
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let subFromMem (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            let res = i - n
            let carry = isCarry7to8 i n
            let half = isHalfCarry3to4 i n
            let z = isZero (uint res)
            let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
            let updatedRegs =
                regs |> Map.add A (B8 (setMin res carry)) // update result in registers
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedRegs,f)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None

let filterSub (r1,(r2 : Resource)) reg mem =
    match r2 with
    | Pointer(Vregister HL) -> subHLpointer reg mem
    | Number N8 -> subFromMem reg mem
    | Register n -> sub8bitRegisters A n reg
    | _ -> None