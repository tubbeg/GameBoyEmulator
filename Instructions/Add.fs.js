import { add, tryFind } from "../fable_modules/fable-library-js.4.17.0/Map.js";
import { Flags } from "../Flags.fs.js";
import { VirtualRegister, updateVirtualReg, translateVirtualReg, regsToVirtual, GameBoyRegister, BitData } from "../Registers.fs.js";
import { printf, toConsole } from "../fable_modules/fable-library-js.4.17.0/String.js";

export function isCarry15to16(x, y) {
    const res = (x + y) >> 16;
    return res > 0;
}

export function isHalfCarry11to12(x, y) {
    const maskedX = x & 4095;
    const maskedY = y & 4095;
    return ((maskedX + maskedY) >> 12) > 0;
}

export function isCarry7to8(x, y) {
    const res = (x + y) >> 8;
    return res > 0;
}

export function isHalfCarry3to4(x, y) {
    const maskedX = x & 15;
    const maskedY = y & 15;
    return ((maskedX + maskedY) >> 4) > 0;
}

export function isZero(n) {
    return n === 0;
}

export function setMax(n, carry) {
    if (carry) {
        return 255;
    }
    else {
        return n;
    }
}

export function add8bitRegisters(a, b, regs) {
    const data1 = tryFind(a, regs);
    const data2 = tryFind(b, regs);
    let matchResult, x, y;
    if (data1 != null) {
        if (data1.tag === 1) {
            if (data2 != null) {
                if (data2.tag === 1) {
                    matchResult = 0;
                    x = data1.fields[0];
                    y = data2.fields[0];
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            const res = x + y;
            const matchValue_1 = isCarry7to8(x, y);
            const half = isHalfCarry3to4(x, y);
            const carry = matchValue_1;
            const z = isZero(res);
            const resMax = setMax(res, carry);
            const f = new Flags(z, false, half, carry);
            const updatedRegisters = add(a, new BitData(1, [resMax]), regs);
            return [updatedRegisters, f];
        }
        default: {
            toConsole(printf("No 8 bit values detected!"));
            return undefined;
        }
    }
}

export function addFromMemory(regs, memory) {
    const a = tryFind(new GameBoyRegister(0, []), regs);
    const pc = tryFind(new GameBoyRegister(8, []), regs);
    let matchResult, adress;
    if (pc != null) {
        if (pc.tag === 0) {
            matchResult = 0;
            adress = pc.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            const n8 = tryFind(adress + 1, memory);
            let matchResult_1, i, n;
            if (n8 != null) {
                if (a != null) {
                    if (a.tag === 1) {
                        matchResult_1 = 0;
                        i = a.fields[0];
                        n = n8;
                    }
                    else {
                        matchResult_1 = 1;
                    }
                }
                else {
                    matchResult_1 = 1;
                }
            }
            else {
                matchResult_1 = 1;
            }
            switch (matchResult_1) {
                case 0: {
                    const res = n + i;
                    const carry = isCarry7to8(n, i);
                    const half = isHalfCarry3to4(n, i);
                    const z = isZero(res);
                    const f = new Flags(z, false, half, carry);
                    const updatedRegs = add(new GameBoyRegister(8, []), new BitData(0, [adress + 1]), add(new GameBoyRegister(0, []), new BitData(1, [res]), regs));
                    return [updatedRegs, f];
                }
                default: {
                    toConsole(printf("Invalid data from memory bus"));
                    return undefined;
                }
            }
        }
        default: {
            toConsole(printf("Invalid adress"));
            return undefined;
        }
    }
}

export function addHL(res, regs) {
    const err = () => {
        toConsole(printf("Invalid add operation. ADD HL needs virtual register as second operand"));
        return undefined;
    };
    toConsole(printf("Got second resource: %A"))(res);
    const hl = regsToVirtual(new GameBoyRegister(6, []), new GameBoyRegister(7, []), regs);
    let matchResult, src, vreg;
    if (res.tag === 2) {
        if (hl != null) {
            matchResult = 0;
            src = hl;
            vreg = res.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            const patternInput = translateVirtualReg(vreg);
            const b = patternInput[1];
            const a = patternInput[0];
            const t = regsToVirtual(a, b, regs);
            if (t != null) {
                const targ = t;
                toConsole(printf("target is %A"))(t);
                const res_1 = targ + src;
                const carry = isCarry15to16(targ, src);
                const half = isHalfCarry11to12(targ, src);
                const z = isZero(res_1);
                const f = new Flags(z, false, half, carry);
                const update = updateVirtualReg(res_1, new VirtualRegister(3, []), regs);
                return [update, f];
            }
            else {
                return err();
            }
        }
        default:
            return err();
    }
}

export function addHLpointer(regs, mem) {
    const a = tryFind(new GameBoyRegister(0, []), regs);
    const hl = regsToVirtual(new GameBoyRegister(6, []), new GameBoyRegister(7, []), regs);
    toConsole(printf("Found hl data %A"))(hl);
    if (hl != null) {
        const adress = hl;
        const data = tryFind(adress, mem);
        let matchResult, b, dataHL;
        if (data != null) {
            if (a != null) {
                if (a.tag === 1) {
                    matchResult = 0;
                    b = a.fields[0];
                    dataHL = data;
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 1;
        }
        switch (matchResult) {
            case 0: {
                const res = b + dataHL;
                const res_1 = b + dataHL;
                const carry = isCarry7to8(b, dataHL);
                const half = isHalfCarry3to4(b, dataHL);
                const z = isZero(res_1);
                const f = new Flags(z, false, half, carry);
                const ur = add(new GameBoyRegister(0, []), new BitData(1, [res_1]), regs);
                return [ur, f];
            }
            default: {
                toConsole(printf("No 8 bit values detected!"));
                return undefined;
            }
        }
    }
    else {
        return undefined;
    }
}

export function addA(res, registers, memory) {
    toConsole(printf("Got resource %A"))(res);
    let matchResult, n;
    switch (res.tag) {
        case 3: {
            if (res.fields[0].tag === 2) {
                if (res.fields[0].fields[0].tag === 3) {
                    matchResult = 0;
                }
                else {
                    matchResult = 3;
                }
            }
            else {
                matchResult = 3;
            }
            break;
        }
        case 0: {
            if (res.fields[0].tag === 1) {
                matchResult = 1;
            }
            else {
                matchResult = 3;
            }
            break;
        }
        case 1: {
            matchResult = 2;
            n = res.fields[0];
            break;
        }
        default:
            matchResult = 3;
    }
    switch (matchResult) {
        case 0:
            return addHLpointer(registers, memory);
        case 1:
            return addFromMemory(registers, memory);
        case 2:
            return add8bitRegisters(new GameBoyRegister(0, []), n, registers);
        default:
            return undefined;
    }
}

export function addSP(regs, memory) {
    const sp = tryFind(new GameBoyRegister(9, []), regs);
    const pc = tryFind(new GameBoyRegister(8, []), regs);
    let matchResult, adress;
    if (pc != null) {
        if (pc.tag === 0) {
            matchResult = 0;
            adress = pc.fields[0];
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            const e8 = tryFind(adress + 1, memory);
            let matchResult_1, i, n;
            if (e8 != null) {
                if (sp != null) {
                    if (sp.tag === 0) {
                        matchResult_1 = 0;
                        i = sp.fields[0];
                        n = e8;
                    }
                    else {
                        matchResult_1 = 1;
                    }
                }
                else {
                    matchResult_1 = 1;
                }
            }
            else {
                matchResult_1 = 1;
            }
            switch (matchResult_1) {
                case 0: {
                    const m = n;
                    const res = m + i;
                    const carry = isCarry15to16(m, i);
                    const half = isHalfCarry11to12(m, i);
                    const f = new Flags(false, false, half, carry);
                    const updatedRegs = add(new GameBoyRegister(8, []), new BitData(0, [adress + 1]), add(new GameBoyRegister(9, []), new BitData(0, [res]), regs));
                    return [updatedRegs, f];
                }
                default:
                    return undefined;
            }
        }
        default:
            return undefined;
    }
}

export function filterAdd(r1, r2, mem, reg) {
    toConsole(printf("Got resources %A %A"))(r1)(r2);
    let matchResult;
    switch (r1.tag) {
        case 1: {
            switch (r1.fields[0].tag) {
                case 0: {
                    matchResult = 0;
                    break;
                }
                case 9: {
                    matchResult = 1;
                    break;
                }
                default:
                    matchResult = 3;
            }
            break;
        }
        case 2: {
            if (r1.fields[0].tag === 3) {
                matchResult = 2;
            }
            else {
                matchResult = 3;
            }
            break;
        }
        default:
            matchResult = 3;
    }
    switch (matchResult) {
        case 0:
            return addA(r2, reg, mem);
        case 1:
            return addSP(reg, mem);
        case 2:
            return addHL(r2, reg);
        default: {
            toConsole(printf("Invalid add operation: %A %A"))(r1)(r2);
            return undefined;
        }
    }
}

