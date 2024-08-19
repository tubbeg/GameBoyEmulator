module Fetch
open ALU
open Register
open Opcodes
open Memory


//NOTE! GB is LITTLE ENDIAN
//Low byte is first!

let notImplemented () =
    eprintfn "NOT YET IMPLEMENTED"
    None

type FetchData =
    | Sh of Short
    | By of Byte
    | Sb of SignedByte

type Fetch =
    {
        registers:Registers;
        operand1:FetchData option;
        operand2:FetchData option;
    }

let boolToBit b =
    match b with
    | true -> 1
    | _ -> 0

let statusToByte (f : ProcessorStatus) =
    let c,h,s,z = f.carry, f.half,f.sub, f.zero
    let cbit,hbit,sbit,zbit =
        boolToBit c, (boolToBit h) <<< 1, (boolToBit s) <<< 2, (boolToBit z) <<< 3
    (zbit ||| sbit ||| hbit ||| cbit) |> intToByte

let createVirtualRegister (r1 : Operand) (r2 : Operand) (r : Registers) =
    let statusByte = statusToByte r.F
    match r1,r2, statusByte with
    | H,L,_ -> bytesToShort r.H r.L |> Short |> Some
    | B,C,_ -> bytesToShort r.B r.C |> Short |> Some
    | D,E,_ -> bytesToShort r.D r.E |> Short |> Some
    | A,F,Some s -> bytesToShort r.A s |> Short |> Some
    | _ -> None

let createVR h l r =
    let vr = createVirtualRegister h l r
    match vr with
    | None -> None
    | Some s -> Some (r,Sh s)

let incPC (r : Registers) : Registers option  =
    match ((r.PC |> shortToInt) + 1) |> intToShort with
    | Some pc ->
        {A=r.A;
        B=r.B;
        C=r.C;
        D=r.D;
        E=r.E;
        F=r.F;
        H=r.H;
        L=r.L;
        SP=r.SP;
        PC=pc} |> Some
    | _ -> None


let fetchImmediateByte r m  =
    match incPC r with
    | Some registers ->
        let data = getDataFromMem (registers.PC) m
        match data with
        | Some d -> Some (registers,By d)
        | _ -> None
    | _ -> None


let fetchImmediateShort registers m : (Registers * FetchData) option =
    match registers |> incPC with
    | Some r ->
        match  incPC r  with
        | Some r2 ->
            match getDataFromMem r2.PC m,getDataFromMem r.PC m with
            | Some high, Some low ->
                let s = bytesToShort high low |> Sh
                Some (r2, s)
            | _ -> None
        | _ -> None
    | _ -> None

let fetchAdressByte registers m =
    let f = fetchImmediateByte registers m
    match f with
    | Some (r,By d) ->
        match (d |> byteToInt) + 0xFF00 |>  intToShort with
        | Some shrt ->
            Some (r,Sh shrt)
        | _ -> None
    | _ -> None
    

let fetchMemory m data  =
    match data with
    | Some (r,Sh s) ->
        let b = getDataFromMem s m
        match b with
        | Some bte -> Some (r, By bte)
        | _ -> None
    | _ -> None

let fetchSignedByte registers m =
    match fetchImmediateByte registers m with
    | Some (r,By d) ->
        //all values are stored as bytes
        //signed only matters during calculation
        Some (r, Sb d)
    | _ -> None

let rec fetchOperand oper (r : Registers) m : (Registers * FetchData) option =
    match oper with
    | A -> Some (r, By r.A)
    | B -> Some (r, By r.B)
    | C -> Some (r, By r.C)
    | D -> Some (r, By r.D)
    | E -> Some (r, By r.E)
    | H -> Some (r, By r.H)
    | L -> Some (r, By r.L)
    | SP -> Some (r, Sh r.SP)
    | HL -> createVR H L r
    | BC -> createVR B C r
    | DE -> createVR D E r
    | AF -> createVR A F r
    | E8 -> fetchSignedByte  r m
    | N8 -> fetchImmediateByte r m
    | A8 -> fetchAdressByte  r m
    | N16 -> fetchImmediateShort  r m
    | Pointer (n) ->
        fetchOperand n r m |> fetchMemory m
    | _ -> None

let createFetchRec r o1 o2 =
    Some {registers=r;operand1=o1;operand2=o2}

let fetch2operands op1 op2 registers m =
    match op1,op2 with
    | Some operand1,Some operand2 ->
        match fetchOperand operand1 registers m with
        | Some (updatedRegisters,firstData) ->
            match fetchOperand operand2 updatedRegisters m with
            | Some (updatedRegisters2, secondData) ->
                createFetchRec updatedRegisters2 (Some firstData) (Some secondData)
            | _ -> None
        | _ -> None
    | _ -> None

let fetch1operand op registers m =
    match op with
    | Some o ->
        match fetchOperand o registers m with
        | Some (r,data) ->
            createFetchRec r (Some data) None
        | _ -> None 
    | _ -> None

let fetch (opParse : OperandParse) r m =
    let i,o1,o2 =
        opParse.instruction,opParse.operand1,opParse.operand2
    match i,o1,o2 with
    | (NOP | HALT),_,_ -> createFetchRec r None None
    | (INC | DEC ),r1,_ ->  fetch1operand r1 r m
    | (ADD | SUB | ADC | SBC | XOR | OR | AND | LD | RES),r1,r2 ->
        fetch2operands r1 r2 r m
    | _,_,_ -> notImplemented ()