module Inc
open Registers
open MemoryBus
open Instructions
open Flags



let isHalfCarryBorrow n instr =
    let mask = n&&&0x0Fuy
    match instr with
    | INC(_) -> (((mask + 1uy) >>> 4) > 0uy)
    | _ -> (((mask - 1uy) >>> 4) > 0uy)

let setMinMax (n : byte)  =
    match (n < 0uy), (n > 255uy) with
    | true,false -> 0uy
    | false, true -> 255uy
    | _ -> n

let merge x y =
    let x16 = ((uint16 x) <<< 8)
    let y16 = (uint16 y)
    x16|||y16

let split n= 
    let x = byte ((0xFF00us&&&n) >>> 8)
    let y = byte (0x00FFus&&&n)
    x,y


let incHLpointer   (regs : RegisterMap) (mem : MemoryBus) instr  =
    let hl : uint16 option = regsToVirtual H L regs
    printf "Found hl data %A" hl
    let pc = regs |> Map.tryFind PC
    let cb = regs |> Map.tryFind F |> isCarry
    printf "PC is: %A" pc
    printf "Memory at PC is: " 
    match hl with
    | Some adress ->
        let data = mem |> Map.tryFind adress
        match data with
        | Some(dataHL) ->
            let res,s =   
                match instr with
                | (INC _) -> (dataHL + 1uy),false
                | _ -> (dataHL - 1uy),true
            let f = {Zero=(res = 0uy);Sub=s;HalfCarry=isHalfCarryBorrow res instr;Carry=cb}
            let (updatedMem : MemoryBus) = res |> storeByte mem adress
            Some(updatedMem,regs,f)
        | _ ->
            printfn "No 8 bit values detected!"
            None
    | _ -> None


let inc8bitRegisters  a  (regs : RegisterMap) mem instr =
    let data1 = regs |> Map.tryFind a
    let cb = regs |> Map.tryFind F |> isCarry
    match data1 with
    | Some(B8 x)-> 
        let res,s =   
            match instr with
            | INC(_) -> x + 1uy,false
            | _ -> x - 1uy,true
        let f = {Zero=(res = 0uy);Sub=s;HalfCarry=isHalfCarryBorrow res instr;Carry=cb}
        let updatedRegisters = regs |> Map.add a (B8 (setMinMax res))
        Some(mem, updatedRegisters,f)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let inc16bitRegisters  v (regs : RegisterMap) mem instr   =
    let a,b = translateVirtualReg v
    let data1 = regs |> Map.tryFind a
    let data2 = regs |> Map.tryFind b
    let flags = regs |> Map.tryFind F |> convertRegToFlagRec
    match data1,data2 with
    | Some(B8 x),Some(B8 y) ->
        let merge = merge x y
        let res =   
            match instr with
            | INC(_) -> (merge + 1us)
            | _ -> (merge - 1us)
        let aUpdate, bUpdate = split res
        let updatedRegisters =
            regs
            |> Map.add a (B8 (setMinMax aUpdate))
            |> Map.add b (B8 (setMinMax bUpdate))
        Some(mem,updatedRegisters,flags)
    | _ ->
        printfn "No 8 bit values detected!"
        None

let incMem (regs : RegisterMap) (memory : MemoryBus) (instr : Instruction) =
    let pc = regs |> Map.tryFind PC
    match pc with
    | Some (B16 adress) ->
        let n8 = memory |> Map.tryFind (adress + 1us)
        match n8 with
        | Some n->
            printf "N8 is %A" n
            let res =   
                match instr with
                | INC(_) -> (n + 1uy)
                | _ -> (n - 1uy)
            let f = {Zero=(res = 0uy);Sub=false;HalfCarry=false;Carry=false}
            let updatedMemory = memory |> Map.add adress res
            let updatedRegs =
                regs 
                |> Map.add PC (B16 (adress + 1us)) //update program counter
            Some(updatedMemory,updatedRegs,f)
        | _ ->
            printf "Invalid data from memory bus"
            None
    | _ ->
        printf "Invalid adress"
        None

let filterIncDec (res : Resource) reg mem (i : Instruction) =
    match res with
    | Pointer(Vregister HL) -> incHLpointer reg mem i
    | Vregister n -> inc16bitRegisters n  reg mem i
    | Register n -> inc8bitRegisters n reg mem i
    | Number N8 -> incMem reg mem i
    | _ -> None