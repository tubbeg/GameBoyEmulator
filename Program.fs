open Instructions
open Registers
open MemoryBus
open Browser
open Processor
open Fable.Core


let div = document.createElement "div"
div.innerHTML <- "Hello world!"
document.body.appendChild div |> ignore

printf "Stuff"

let key : uint16 = 32us
let mem : MemoryBus = Map.empty |> Map.add key 15uy

let readFirstByte mem registers : byte option =
    let pc = getPCregister registers
    match pc with
    | Some adress -> mem |> Map.tryFind adress
    | _ -> None

let mapByteToInstruction byte =
    match byte with
    | Some b -> InstructionOpCodeMap |> Map.tryFind b
    | _ -> None 


let execute instruction memory registers =
    match instruction with
    | Some(i) -> ""
    | None ->  ""

let MainCPULoop (memory : MemoryBus) (registers : Register list) =
    let rec CPULoop memory registers =
        let byte = readFirstByte memory registers
        let instr = mapByteToInstruction byte
        0
    0



