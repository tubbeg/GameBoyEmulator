module MemoryBus
open Registers

type MemoryBus = Map<uint16,byte>


let storeByte (m : MemoryBus) adress data =
     m |> Map.add adress data

let readByte (m : MemoryBus) (adress : BitData) : byte option =
     match adress with
     | B16 a ->  m |> Map.tryFind a
     | _ -> None