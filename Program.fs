open Instructions
open Registers
open MemoryBus
open Browser
open Processor
open Fable.Core
open Sub
open Add
open And
open SubC
open AddC
open Or
open And

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
        | SUB (a, b) -> filterSub (a,b) registers memory
        | SBC (a, b) -> filterSubC (a,b ) registers memory
        | ADC (a, b) -> filterAddC (a,b ) registers memory
        | AND (a, b) -> filterAnd (a,b ) registers memory
        | OR (a, b) -> filterOrXor (a,b ) registers memory i
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


let testSub() =
    let testSubReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x90uy
        let r =
            registers |> Map.add B (B8 0xFFuy) |> Map.add A (B8 0xFFuy)
        MainCPULoop mem r
    let subN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xD6uy |> Map.add 1337us 0x04uy
        let r = registers |> Map.add PC (B16 1336us) |> Map.add A (B8 0x06uy)
        MainCPULoop mem r
    let testSubHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x96uy |> Map.add 1337us 0x02uy
        let r = registers |> updateVirtualReg 1337us HL  |> Map.add A (B8 0x06uy)
        MainCPULoop mem r
    0
    

let testSubc() =
    let testSubCReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x98uy
        let r =
            registers |> Map.add B (B8 0xFEuy) |> Map.add A (B8 0xFFuy) |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    let subCN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xDEuy |> Map.add 1337us 0x04uy
        let r =
            registers |> Map.add PC (B16 1336us) |> Map.add A (B8 0x06uy) |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    let testSubCHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x9Euy |> Map.add 1337us 0x02uy
        let r =
            registers |> updateVirtualReg 1337us HL  |> Map.add A (B8 0x06uy)  |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    0
    


let testAddc() =
    let testAddCReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x88uy
        let r =
            registers |> Map.add B (B8 0xFEuy) |> Map.add A (B8 0xFFuy) |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    let addCN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xCEuy |> Map.add 1337us 0x04uy
        let r =
            registers |> Map.add PC (B16 1336us) |> Map.add A (B8 0x06uy) |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    let testAddCHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0x8Euy |> Map.add 1337us 0x02uy
        let r =
            registers |> updateVirtualReg 1337us HL  |> Map.add A (B8 0x06uy)  |> Map.add F (B8 0x10uy)
        MainCPULoop mem r
    0

let testAnd() =
    let testAndReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xA0uy
        let r =
            registers |> Map.add B (B8 0x0Fuy) |> Map.add A (B8 0xFFuy)
        MainCPULoop mem r
    let andN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xE6uy |> Map.add 1337us 0x00uy
        let r =
            registers |> Map.add PC (B16 1336us) |> Map.add A (B8 0x0Fuy)
        MainCPULoop mem r
    let testAndHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xA6uy |> Map.add 1337us 0x02uy
        let r =
            registers |> updateVirtualReg 1337us HL  |> Map.add A (B8 0x06uy)
        MainCPULoop mem r
    0


    
let testOr =
    let testOrReg = 
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xB0uy
        let r =
            registers |> Map.add B (B8 0x0Fuy) |> Map.add A (B8 0xF0uy)
        MainCPULoop mem r
    let OrN8 =
        let addr = 1336us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xF6uy |> Map.add 1337us 0xF0uy
        let r =
            registers |> Map.add PC (B16 1336us) |> Map.add A (B8 0x0Fuy)
        MainCPULoop mem r
    let testOrHLpointer =
        let addr = 0us
        let mem : MemoryBus = Map.empty |> Map.add addr 0xB6uy |> Map.add 1337us 0x03uy
        let r =
            registers |> updateVirtualReg 1337us HL  |> Map.add A (B8 0x0Cuy)
        MainCPULoop mem r
    0


    