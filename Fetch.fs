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

type Fetch =
    {
        registers:Registers;
        operand1:FetchData option;
        operand2:FetchData option;
    }

let statusToByte (f : Status) =
    let z,s,h,c =
        boolToBit f.zero, boolToBit f.sub,
        boolToBit f.half, boolToBit f.carry
    let zbit = z <<< 3
    let sbit = s <<< 2
    let hbit = h <<< 1
    (zbit ||| sbit ||| hbit ||| c) |> Byte


let createVirtualRegister (r1 : Operand) (r2 : Operand) (r : Registers) =
    match r1,r2 with
    | H,L -> bytesToShort r.H r.L |> Short |> Some
    | B,C -> bytesToShort r.B r.C |> Short |> Some
    | D,E -> bytesToShort r.D r.E |> Short |> Some
    | A,F -> bytesToShort r.A (statusToByte r.F) |> Short |> Some
    | _ -> error (r1,r2)

let createVR h l r =
    let vr = createVirtualRegister h l r
    match vr with
    | None -> None
    | Some s -> Some (r,Sh s)

let incPC (r : Registers) =
    let pc = (r.PC.shortValue + 1) |> Short
    {A=r.A;
    B=r.B;
    C=r.C;
    D=r.D;
    E=r.E;
    F=r.F;
    H=r.H;
    L=r.L;
    SP=r.SP;
    PC=pc}


let fetchImmediateByte r m  =
    let registers = incPC r
    let data = getDataFromMem r.PC m
    match data with
    | Some d -> Some (registers,By d)
    | _ -> None


let fetchImmediateShort registers m : (Registers * FetchData) option =
    let firstInc = incPC registers
    let lowByte = getDataFromMem firstInc.PC m
    let secondInc = incPC firstInc 
    let highByte = getDataFromMem secondInc.PC m
    match highByte,lowByte with
    | Some high, Some low ->
        let s = bytesToShort high low |> Short
        Some (secondInc, Sh s)
    | _ -> None

let fetchAdressByte registers m =
    let f = fetchImmediateByte registers m
    match f with
    | Some (r,By d) ->
        let adress = d.byteValue + 0xFF00 |>  Short
        Some (r,Sh adress)
    | _ -> None
    

let fetchMemory m data  =
    match data with
    | Some (r,Sh s) ->
        let b = getDataFromMem s m
        match b with
        | Some bte -> Some (r, By bte)
        | _ -> None
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
    | N8 -> fetchImmediateByte r m
    | A8 -> fetchAdressByte r m
    | N16 -> fetchImmediateShort r m
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
    | NOP,_,_ -> createFetchRec r None None
    | HALT,_,_ -> createFetchRec r None None
    | INC,r1,_ ->  fetch1operand r1 r m
    | DEC,r1,_ ->  fetch1operand r1 r m
    | ADD,r1,r2 -> fetch2operands r1 r2 r m
    | _,_,_ -> None