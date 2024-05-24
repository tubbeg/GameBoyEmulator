module Fetch
open Registers
open MemoryBus
open Instructions



let incrementPC (r : RegisterMap option) =
    match r with
    | Some registers ->
        let current = registers |> Map.tryFind PC
        match current with
        | Some(B16 n) ->  registers |> Map.add PC (B16 (n + 1us)) |> Some
        | _ -> None
    | _ -> None

let readCurrentMemory (m : MemoryBus) (r : RegisterMap option) =
    match r with
    | Some registers -> 
        let currentPC = registers |> Map.tryFind PC
        match currentPC with
        | Some(B16 adress) ->
            m |> Map.tryFind adress
        | _ -> None
    | _ -> None

let mergeBytes (high : byte option) low =
    match high, low with
    | Some(highByte), Some(lowByte) ->
        let h = (uint16 highByte) <<< 8
        let l = (uint16 lowByte)
        Some(h ||| l)
    | _ -> None

let handleImmediate n rm m =
    match n with
    | N8 | E8 | A8 ->
        let updatedRegisters = incrementPC rm
        let data = readCurrentMemory m updatedRegisters
        match data with
        | None -> None,updatedRegisters, None
        | Some(d) -> Some(B8 d),updatedRegisters, None
    | A16 | N16 ->
        let updatedRegisters = (incrementPC rm |> incrementPC)
        let high = readCurrentMemory m (incrementPC rm)
        let low = readCurrentMemory m (incrementPC rm |> incrementPC)
        let data = mergeBytes high low
        match data with
        | None -> None, updatedRegisters, None
        | Some(d) -> Some(B16 d), updatedRegisters, None

let handleRegister (rm : RegisterMap option) n =
    match rm with
    | Some registers -> registers |> Map.tryFind n
    | _ -> None

let handleVirtualRegister (rm : RegisterMap option) n =
    match rm with
    | Some registers ->
        let a,b = translateVirtualReg n
        let high = registers |> Map.tryFind a
        let low = registers |> Map.tryFind b
        match high, low with
        | Some(B8 h), Some(B8 l) ->
            let merge = mergeBytes (Some h) (Some l)
            match merge with
            | Some(d) -> Some(B16 d), rm,None
            | _ -> None,None,None
        | _ -> None, None,None
    | _ -> None,None,None


//Returns the resource value, updated registers, and optinally an adress, in case it's a pointer
let rec fetchResource (r : Resource) (rm : RegisterMap option) (m : MemoryBus) =
    match r with
    | Number n -> handleImmediate n rm m
    | Register n -> (handleRegister rm n), rm,None
    | Vregister n -> handleVirtualRegister rm n
    | Pointer res ->
        let a,regs,_ = fetchResource res rm m
        let data =
            match a with
            | Some(B16 adress) -> m |> Map.tryFind adress
            | _ -> None
        match data with
        | Some(d) -> Some(B8 d), regs, a
        | _ -> None, regs, None

let splitShort n =
    let high = (n&&&0xFF00us) >>> 8
    let low = (n&&&0x00FFus)
    (byte high), (byte low)

let updateResource r (rm : RegisterMap option) (m : MemoryBus) (v : BitData) (adress : BitData option) =
    match rm with
    | Some registers ->
        match r,v,adress with
        | Register register, (B8 n),_ ->
            let regs = registers |> Map.add register (B8 n)
            Some(regs,m)
        | Vregister vreg, (B16 n),_ ->
            let a,b = translateVirtualReg vreg
            let high,low = splitShort n
            let regs = registers |> Map.add a (B8 high) |> Map.add b (B8 low)
            Some(regs,m)
        | Pointer _,(B8 n),Some(B16 a) ->
            let mem = m |> Map.add a n
            Some(registers,mem)
        | Number _,_,_ | _ ->
            //Immediates cannot be updated obviously, unless they are memory pointers
            //but then we get the adress from a separate parameter. There's no need to decrement PC to
            //determine the adress
            eprintf "INCORRECT UPDATE: %A %A %A %A %A" r rm m v adress
            None
    | _ -> None