module Register

type Status = 
    {
        zero:bool;
        sub:bool;
        half:bool;
        carry:bool
    }

type Byte (value) =
    class
        member this.byteValue = value % 255
    end

type Short (value) =
    class
        member this.shortValue = value % (0xFFFF)
    end

//js does not support byte or nibble. Only double-precision 64-bit binary
type Registers =
    {
        A:Byte
        B:Byte
        C:Byte
        D:Byte
        E:Byte
        F:Status
        H:Byte
        L:Byte
        SP:Short
        PC:Short
    }




