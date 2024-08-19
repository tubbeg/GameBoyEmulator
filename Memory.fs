module Memory
open Register
open ALU


type Memory = Map<int,Byte>

let inRange n =
    ((n < 0x10000) && (n >= 0))

let getDataFromMem (k : Short) (m : Memory) : Byte option =
    let i = k |> shortToInt
    match i with
    | n when inRange n -> 
        let b = m |> Map.tryFind i
        match b with
        | None -> Some (N 0,N 0)
        | _ -> b
    | _ ->
        eprintfn "Memory Out of Range: %A" i
        None

let updateMemory  (k : Short) (v : Byte) (m : Memory) : Memory =
    m |> Map.add (k |> shortToInt) v 
    