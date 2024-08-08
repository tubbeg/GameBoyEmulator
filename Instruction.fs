module Instruction
open Register
open Memory
open ALU
open Opcodes
open Fetch

let updateAccumulatorAndStatus r out  : Registers  =
    {A=out.y;
     B=r.B;
     C=r.C;
     D=r.D;
     E=r.E;
     F=out.s;
     H=r.H;
     L=r.L;
     SP=r.SP;
     PC=r.PC}

let clearCarry r  =
    let s = r.F
    let clear = {zero=s.zero;sub=s.sub;half=s.half;carry=false}
    {A=r.A;
     B=r.B;
     C=r.C;
     D=r.D;
     E=r.E;
     F=clear;
     H=r.H;
     L=r.L;
     SP=r.SP;
     PC=r.PC}


let addBytes a b registers : Registers option  = 
    match alu {a=a;b=b;s=registers.F;i=ADD} with
    | Some out -> updateAccumulatorAndStatus registers out |> Some
    | _ -> None

let adcBytes a b registers =
    match alu {a=a;b=b;s=registers.F;i=ADC} with
    | Some out -> updateAccumulatorAndStatus registers out |> Some
    | _ -> None

let addShorts (h : Short) (l : Short) (f2 : Fetch) =
    let ah,al = h.highByte,h.lowByte
    let bh,bl = l.highByte,l.lowByte
    match addBytes al bl (clearCarry f2.registers) with
    | Some r -> adcBytes ah bh r  
    | _ -> None

let addInstruction f1 f2 : Registers option  =
    match f1.data,f2.data with
    | By h, By l -> addBytes h l f2.registers
    | Sh h, Sh l -> addShorts h l f2
    | _ -> None