module SubC
open Registers
open MemoryBus
open Instructions
open Flags




let isCarry7to8 (x : byte) (y : byte) cb =
    let res =( (x - y) - cb) >>> 8
    res > 0uy

let isHalfCarry3to4 (x :byte) (y:byte) cb =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    (((maskedX - maskedY) - cb) >>> 4) > 0uy

let isZero (n : uint) =
    n = 0u

let setMin (n : byte) carry : byte =
    match carry, (n > 0uy) with
    | true,_ -> 0uy
    | _,false -> 0uy
    | false,true -> n


let subCHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    let cb = regs |> Map.tryFind F |> getCarryBit
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = (b - dataHL) - cb
            let carry = isCarry7to8 b dataHL cb
            let half = isHalfCarry3to4 b dataHL cb
            let z = isZero (uint res)
            let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
            let ur = regs |> Map.add A (B8 (setMin res carry))
            Some(ur,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let subC8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    let cb = regs |> Map.tryFind F |> getCarryBit
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = (x - y) - cb
        let carry, half = isCarry7to8 x y cb, isHalfCarry3to4 x y cb
        let z = isZero (uint res)
        let resMin = setMin res carry
        let f = {Zero=z;Sub=true;HalfCarry=half;Carry=carry}
        let updatedRegisters = regs |> Map.add a (B8 resMin)
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let subCFromMemory (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    let cb = regs |> Map.tryFind F |> getCarryBit
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            let res = (i - n) - cb
            let carry = isCarry7to8 i n cb
            let half = isHalfCarry3to4 i n cb
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

let filterSubC (r1,(r2 : Resource)) reg mem =
    match r2 with
    | Pointer(Vregister HL) -> subCHLpointer reg mem
    | Number N8 -> subCFromMemory reg mem
    | Register n -> subC8bitRegisters A n reg
    | _ -> None