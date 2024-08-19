module Utility
open Register
open Opcodes
open Fetch
open ALU


let updateStatus  v registers  =
    match registers with
    | Some r -> 
        {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=v;H=r.H;L=r.L;SP=r.SP;PC=r.PC} |> Some
    | _ -> None

let updateRegister (key : Operand) v register  =
    match key,v,register with
    | Operand.A,By value,Some r  -> 
        Some {A=value;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | Operand.B,By value,Some r -> 
        Some {A=r.A;B=value;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | Operand.C,By value,Some r -> 
        Some {A=r.A;B=r.B;C=value;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | Operand.D,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=value;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | Operand.E,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=value;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=r.PC}
    | H,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=value;L=r.L;SP=r.SP;PC=r.PC}
    | L,By value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=value;SP=r.SP;PC=r.PC}
    | SP,Sh value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=value;PC=r.PC}
    | PC,Sh value,Some r -> 
        Some {A=r.A;B=r.B;C=r.C;D=r.D;E=r.E;F=r.F;H=r.H;L=r.L;SP=r.SP;PC=value}
    | _ -> None


let updateRegisterOption (key : Operand option) v (register : Registers)  =
    match key with
    | Some k -> updateRegister k v (Some register)
    | _ -> None

let clearCarry r  =
    updateStatus {zero=r.F.zero;sub=r.F.sub;half=r.F.half;carry=false}

let updateSP r sp   =
    updateRegister SP sp r

let updateAF a f r =
    //status is low byte according to docs
    None

let updateBC high low r =
    r |>
    updateRegister Operand.B (By high) |>
    updateRegister Operand.C (By low)

let updateDE high low r =
    r |>
    updateRegister Operand.D (By high) |>
    updateRegister Operand.E (By low)

let updateHL high low r =
    r |>
    updateRegister H (By high) |>
    updateRegister L (By low)


let processorStatusToALUstatus s : Status =
    ZERO s.zero, Sub.SUB s.sub, CARRY s.carry

let aluStatusToProcessorStatus aluOutLow aluOutHigh =
    let hs = aluOutHigh.s
    let ls = aluOutLow.s
    match hs,ls with
    | (ZERO z, Sub.SUB s, CARRY c),(_,_,CARRY h) ->
        {zero=z;sub=s;half=h;carry=c}
