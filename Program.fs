open Instructions
open Registers
open MemoryBus
open Browser
open Processor
open Fable.Core
open Sub
open Add

let div = document.createElement "div"
div.innerHTML <- "Hello world!"
document.body.appendChild div |> ignore


let readFirstByte (mem : MemoryBus) registers : byte option =
    let pc = getPCregister registers
    match pc with
    | Some adress ->
        readByte mem adress 
    | _ -> None

let mapByteToInstruction byte =
    match byte with
    | Some b ->
        let i = InstructionOpCodeMap |> Map.tryFind b
        printf "Found instruction: %A" i
        i
    | _ -> None 

let execute (instruction : Instruction option) (memory : MemoryBus) (registers : RegisterMap) =
    match instruction with
    | Some(i) -> 
        match i with
        | ADD (a,b) -> filterAdd (a,b) memory registers 
        | SUB (a, b) -> filterSub (a,b) registers
        | _ ->
            printf "Instruction %A Not Yet Implemented" i
            None
    | None ->  None

let MainCPULoop (memory : MemoryBus) (registers : RegisterMap) =
    let rec CPULoop (memory : MemoryBus) registers =
        let byte = readFirstByte memory registers
        let instr = mapByteToInstruction byte
        let res = execute instr memory registers
        match res with
        | Some (m,f) ->
            printf "Result is %A %A" m f
        | None ->
            printf "Error"
    CPULoop memory registers


let testAddOperations() =
    let testAddReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x80uy
        let r =
            registers |> Map.add B (B8 0xFFuy) |> Map.add A (B8 0xFFuy)
        MainCPULoop mem r
    let testAddHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x86uy |> Map.add 1337us 0x02uy
        let r = registers |> updateVirtualReg 1337us HL
        MainCPULoop mem r
    let addN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xC6uy |> Map.add 1337us 0x04uy
        let r = registers |> Map.add PC (B16 1336us)
        MainCPULoop mem r
    let addE8 =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xE8uy |> Map.add 0x01us 0x8uy
        let r = registers 
        MainCPULoop mem r
    let addHL =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x09uy
        let r = registers |> updateVirtualReg 0x1000us BC 
        MainCPULoop mem r
    0


let testSub =
    let testSubReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x90uy
        let r =
            registers |> Map.add B (B8 0xFFuy) |> Map.add A (B8 0xFFuy)
        MainCPULoop mem r
    0