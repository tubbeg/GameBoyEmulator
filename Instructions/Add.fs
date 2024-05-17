module Add
open Registers
open MemoryBus
open Instructions
open Flags


//in .NET, the result of carry
//becomes: total overflow -1
//but numbers in js aren't actually bytes,
//they are double precision 64 bit floating point
//64 bits is plenty space for overflow
//this means that we can't verify in the same way

let isCarry15to16 (x : uint16) (y : uint16) =
    let res = (x + y) >>> 16
    res > 0us

let isHalfCarry11to12 (x :uint16) (y:uint16) =
    let maskedX =  x&&&0x0FFFus
    let maskedY =  y&&&0x0FFFus
    ((maskedX + maskedY) >>> 12) > 0us

let isCarry7to8 (x : byte) (y : byte) =
    let res = (x + y) >>> 8
    res > 0uy

let isHalfCarry3to4 (x :byte) (y:byte) =
    let maskedX =  x&&&0x0Fuy
    let maskedY =  y&&&0x0Fuy
    ((maskedX + maskedY) >>> 4) > 0uy

let isZero (n : uint) =
    n = 0u

let setMax n carry =
    match carry with
    | true -> 0xFFuy
    | false -> n

let add8bitRegisters  a b (regs : RegisterMap) :( RegisterMap * Flags) option  =
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    match data1,data2 with
    | Some(B8 x),Some(B8 y) -> 
        let res = x + y
        let carry, half = isCarry7to8 x y, isHalfCarry3to4 x y
        let z = isZero (uint res)
        let resMax = setMax res carry
        let f = {Zero=z;Sub=false;HalfCarry=half;Carry=carry}
        let updatedRegisters = regs |> Map.add a (B8 resMax)
        Some(updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let addFromMemory (regs : RegisterMap) (memory : MemoryBus) =
    let a = regs |> Map.tryFind A
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8,a with
        | Some n, Some (B8 i) ->
            let res = n + i
            let carry = isCarry7to8 n i
            let half = isHalfCarry3to4 n i
            let z = isZero (uint res)
            let f = {Zero=z;Sub=false;HalfCarry=half;Carry=carry}
            let updatedRegs =
                regs |> Map.add A (B8 res) // update result in registers
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedRegs,f)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None
    

let addHL (res : Resource) (regs : RegisterMap) =
    let err () =
        printf "Invalid add operation. ADD HL needs virtual register as second operand"
        None
    printf "Got second resource: %A" res
    let hl = regsToVirtual H L regs
    match res, hl with
    | (Vregister  vreg), Some(src) ->
        let a,b = translateVirtualReg vreg
        let t =  regsToVirtual a b regs
        match t with
        | Some targ ->
            printf "target is %A" t
            let res = targ + src
            let carry = isCarry15to16 targ src
            let half = isHalfCarry11to12 targ src
            let z = isZero (uint res)
            let f = {Zero=z;Sub=false;HalfCarry=half;Carry=carry}
            let update = updateVirtualReg res HL regs
            Some(update,f)
        | _ ->
            err()
    | _ ->
        err()


let addHLpointer   (regs : RegisterMap) (mem : MemoryBus)  =
    let a = regs |> Map.tryFind A
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data,a with
        | Some(dataHL), Some(B8 b) ->
            let res = b + dataHL
            let res = b + dataHL
            let carry = isCarry7to8 b dataHL
            let half = isHalfCarry3to4 b dataHL
            let z = isZero (uint res)
            let f = {Zero=z;Sub=false;HalfCarry=half;Carry=carry}
            let ur = regs |> Map.add A (B8 res)
            Some(ur,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None

let addA res (registers : RegisterMap) (memory : MemoryBus) =
    printf "Got resource %A" res
    match res with 
    | Pointer(Vregister HL) -> addHLpointer registers memory
    | Number N8 -> addFromMemory registers memory
    | Register n -> add8bitRegisters A n registers
    | _ -> None

let addSP (regs : RegisterMap) (memory : MemoryBus) =
    let sp = regs |> Map.tryFind SP
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let e8 = memory |> Map.tryFind (adress + 1us)
        match e8,sp with
        | Some n, Some (B16 i) ->
            let m = uint16 n
            let res = m + i
            let carry = isCarry15to16 m i
            let half = isHalfCarry11to12 m i
            //let z = isZero (byte res)
            let f = {Zero=false;Sub=false;HalfCarry=half;Carry=carry}
            let updatedRegs =
                regs 
                |> Map.add SP (B16 res)
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedRegs,f)
        | _ -> None
    | _ -> None
    

let filterAdd (r1 : Resource,r2) mem reg =
    printf "Got resources %A %A" r1 r2
    match r1 with
    | Register A ->  addA r2 reg mem
    | Register SP ->  addSP  reg mem
    | Vregister HL ->  addHL r2 reg 
    | _ ->
        printf "Invalid add operation: %A %A" r1 r2
        None

