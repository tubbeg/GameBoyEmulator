module MemoryBus

type MemoryBus = Map<uint16,byte>


let storeByte (m : MemoryBus) adress data =
     m |> Map.add adress data

let readByte (m : MemoryBus) adress =
    m |> Map.tryFind adress