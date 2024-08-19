module Register
open ALU

type Byte =
    Nibble * Nibble

type Short =
    Byte * Byte
type SignedByte = Byte

type ProcessorStatus =
    {zero:bool;
    sub:bool;
    half:bool;
    carry:bool}

//js does not support byte or nibble. Only double-precision 64-bit binary
//this is why I have defined my own datatypes
type Registers =
    {
        A:Byte
        B:Byte
        C:Byte
        D:Byte
        E:Byte
        F:ProcessorStatus
        H:Byte
        L:Byte
        SP:Short
        PC:Short
    }




let byteToInt b =
    let h,l = b
    let high,low = nibbleToInt h, nibbleToInt l
    (high <<< 4) ||| low

let signedByteToInt sb =   
    (byteToInt sb) - 128

let shortToInt s =
    let h,l = s
    let high,low = byteToInt h,byteToInt l
    (high <<< 8) ||| low

let bytesToShort high low : Short =
    (high,low)

let intToByte i : Byte option =
    let h,l = (i >>> 4), i &&& 0x0F
    match intToNibble h, intToNibble l with
    | Some a, Some b -> (a,b) |> Some
    | _ -> None

let intToShort i : Short option =
    let h,l = (i >>> 8), i &&& 0x00FF
    match intToByte h, intToByte l with
    | Some a, Some b -> (a,b) |> Some
    | _ -> None