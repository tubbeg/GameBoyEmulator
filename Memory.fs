module Memory
open Register


type Memory = Map<int,Byte>


let getDataFromMem (k : Short) (m : Memory) : Byte option =
    let i = k.shortValue
    m |> Map.tryFind i

let updateMemory  (k : Short) (v : Byte) (m : Memory) : Memory =
    let i = k.shortValue
    m |> Map.add i v 
    