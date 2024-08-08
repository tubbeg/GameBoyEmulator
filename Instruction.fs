module Instruction
open Register
open Memory
open ALU
open Opcodes
open Fetch


let updateRegister (key : Operand) v register  =
    match key,v,register with
    | A,By value,Some r  -> 
        Some {A=value;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | B,By value,Some r -> 
        Some {A=r.A;B=value;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | C,By value,Some r -> 
        Some {A=r.A;B=r.B;C=value;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | D,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=value;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | E,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=value;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | F,By value,Some r  -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=value;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | H,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=value;L=r.L;SP=r.SP;PC=r.PC}
    | L,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=value;SP=r.SP;PC=r.PC}
    | SP,Sh value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=value;PC=r.PC}
    | PC,Sh value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=value}
    | _ -> None

let updateStatus status r =
    r |>
    updateRegister F (By status)

let updateAccumulatorAndStatus out r  =
    r |>
    updateRegister A (By out.y) |>
    updateRegister F (By out.s)

let clearCarry r  =
    let s = r.F
    let clear = s.byteValue &&& 0xFE |> Byte
    Some r |> updateRegister F (By clear)

let updateSP r sp   =
    updateRegister SP sp r

let updateAF a f r =
    //status is low byte according to docs
    r |>
    updateRegister A (By a) |>
    updateRegister F (By f) 

let updateBC high low r =
    r |>
    updateRegister B (By high) |>
    updateRegister C (By low)

let updateDE high low r =
    r |>
    updateRegister D (By high) |>
    updateRegister E (By low)

let updateHL high low r =
    r |>
    updateRegister H (By high) |>
    updateRegister L (By low)


let addBytesToAcc a b registers : Registers option  = 
    match alu {a=a;b=b;s=registers.F;i=ADD} with
    | Some out -> registers |> Some |> updateAccumulatorAndStatus  out
    | _ -> None

let subBytesToAcc a b registers : Registers option  = 
    match alu {a=a;b=b;s=registers.F;i=SUB} with
    | Some out -> registers |> Some |> updateAccumulatorAndStatus  out
    | _ -> None

let adcBytesToAcc a b registers =
    match alu {a=a;b=b;s=registers.F;i=ADC} with
    | Some out -> registers |> Some |> updateAccumulatorAndStatus  out
    | _ -> None

let addShortToVR (p : OperandParse) (vr : Short) (sh : Short) (f : Fetch) =
    let ah,al = vr.highByte,vr.lowByte
    let (bh: Byte),bl = sh.highByte,sh.lowByte
    match p.operand1 with
    | Some vrOp ->
        match alu {a=al;b=bl;s=f.registers.F;i=ADD} with
        | Some out ->
            match alu {a=ah;b=bh;s=out.s;i=ADC} with
            | Some out2 ->
                let h,l = out2.y,out.y
                match vrOp with
                | HL ->
                    f.registers |> Some |>
                    updateHL h l  |>
                    updateStatus out2.s
                | BC ->
                    f.registers |> Some |>
                    updateBC h l |>
                    updateStatus out2.s
                | DE ->
                    f.registers |> Some |>
                    updateDE h l |>
                    updateStatus out2.s
                | _ -> None
            | _ -> None  
        | _ -> None
    | _ -> None

let addSignedByteToSP (sh : Short) (e8 : Sbyte) f =
    match e8.sbyteValue with
    | n when (n < 0) ->
        let toPositiveNumber = (n * (-1))
        match alu {a=sh.lowByte;b=Byte toPositiveNumber;s=f.registers.F;i=SUB} with
        | Some out ->
            (out.y.byteValue ||| sh.shortValue) |> Short |> Sh |>
            updateSP (Some f.registers) |> updateStatus out.s
        | _ -> None
    | p ->
        eprintfn "Check that the flags are correct here."
        eprintfn "overflow from bit 7 according to docs. But SP"
        eprintfn "is still a short! Overflow is possible"
        match alu {a=sh.lowByte;b=Byte p;s=f.registers.F;i=ADD} with
        | Some out ->
            //Adding a zero with carry to prevent potential error
            match alu {a=sh.highByte;b=Byte 0;s=out.s;i=ADC} with
            | Some out2 ->
                bytesToShort out2.y out.y |> Short |> Sh |>
                updateSP (Some f.registers) |> updateStatus out.s
            | _ -> None
        | _ -> None


let addInstruction (parse : OperandParse) (fetch : Fetch) : Registers option  =
    match parse.operand1,fetch.operand1,fetch.operand2 with
    | Some A,Some (By h), Some (By l) -> addBytesToAcc h l fetch.registers
    | Some SP,Some (Sh h), Some (Sb d) -> addSignedByteToSP h d fetch
    | Some (HL | AF | BC | DE), Some (Sh h), Some (Sh l) ->
        addShortToVR parse h l fetch
    | _ -> None

let subInstruction (fetch : Fetch) : Registers option  =
    match fetch.operand1,fetch.operand2 with
    | Some (By h), Some (By l) -> subBytesToAcc h l fetch.registers
    | _ -> None