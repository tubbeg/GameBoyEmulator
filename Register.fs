module Register


let bit8mod value =
    match value with
    | n when (n >= 0 && n < 256) -> n
    | n when (n > 255) -> n % 256
    | n -> 
        match (n % 256) with
        | 0 -> 0
        | i -> i + 256

type Byte (value) =
    class
        let v = value
        member this.byteValue = bit8mod v
        member this.lowNibble = this.byteValue &&& 0x0F
        member this.highNibble = this.byteValue &&& 0xF0
        member this.getBit n =
            match n with
            | 0 -> this.byteValue &&& 0x01 |> Some
            | 1 -> this.byteValue &&& 0x02 |> Some
            | 2 -> this.byteValue &&& 0x04 |> Some
            | 3 -> this.byteValue &&& 0x08 |> Some
            | _ -> None
    end

let shortToBytes (s : int) =
    Byte((s &&& 0xFF00) >>> 8), Byte (s &&& 0x00FF)
    
let bytesToShort (h : Byte) (l : Byte) =
    let high,low = h.byteValue, l.byteValue
    let bitshift = high <<< 8
    (bitshift ||| low )

type Short (value) =
    class
        let high,low = shortToBytes value
        member this.shortValue = bytesToShort high low
        member this.lowByte = low
        member this.highByte = high
    end

let isSigned b = 
    match b &&& 0x80 with
    | 0x80 -> true
    | _ -> false

let byteToSigned (b : int) =
    match isSigned b with
    | true -> (b &&& 0x7F) - 128
    | _ -> b


type Sbyte (value) =
    class
        //this might not actually work because of modulo operator
        let b = Byte value
        member this.sbyteValue = b.byteValue |> byteToSigned
        member this.lowNibble = this.sbyteValue &&& 0x0F
        member this.highNibble = this.sbyteValue &&& 0xF0
    end

//js does not support byte or nibble. Only double-precision 64-bit binary
type Registers =
    {
        A:Byte
        B:Byte
        C:Byte
        D:Byte
        E:Byte
        F:Byte
        H:Byte
        L:Byte
        SP:Short
        PC:Short
    }

let byteToStatus =
    0

    




    
