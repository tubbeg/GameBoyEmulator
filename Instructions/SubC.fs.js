import { add, tryFind } from "../fable_modules/fable-library-js.4.17.0/Map.js";
import { BitData, regsToVirtual, GameBoyRegister } from "../Registers.fs.js";
import { Flags, getCarryBit } from "../Flags.fs.js";
import { printf, toConsole } from "../fable_modules/fable-library-js.4.17.0/String.js";

export function isCarry7to8(x, y, cb) {
    const res = ((x - y) - cb) >> 8;
    return res > 0;
}

export function isHalfCarry3to4(x, y, cb) {
    const maskedX = x & 15;
    const maskedY = y & 15;
    return (((maskedX - maskedY) - cb) >> 4) > 0;
}

export function isZero(n) {
    return n === 0;
}

export function setMin(n, carry) {
    const matchValue = n > 0;
    if (carry) {
        return 0;
    }
    else if (matchValue) {
        return n;
    }
    else {
        return 0;
    }
}

export function subCHLpointer(regs, mem) {
    const a = tryFind(new GameBoyRegister(0, []), regs);
    const hl = regsToVirtual(new GameBoyRegister(6, []), new GameBoyRegister(7, []), regs);
    const cb = getCarryBit(tryFind(new GameBoyRegister(5, []), regs));
    toConsole(printf("Found hl data %A"))(hl);
    if (hl != null) {
        const adress = hl;
        const data = tryFind(adress, mem);
        let matchResult, b_1, dataHL;
        if (data != null) {
            if (a != null) {
                if (a.tag === 1) {
                    matchResult = 0;
                    b_1 = a.fields[0];
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
                const res = (b_1 - dataHL) - cb;
                const carry = isCarry7to8(b_1, dataHL, cb);
                const half = isHalfCarry3to4(b_1, dataHL, cb);
                const z = isZero(res);
                const f = new Flags(z, true, half, carry);
                const ur = add(new GameBoyRegister(0, []), new BitData(1, [setMin(res, carry)]), regs);
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

export function subC8bitRegisters(a, b, regs) {
    const data1 = tryFind(a, regs);
    const data2 = tryFind(b, regs);
    const cb = getCarryBit(tryFind(new GameBoyRegister(5, []), regs));
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
            const res = (x - y) - cb;
            const matchValue_1 = isCarry7to8(x, y, cb);
            const half = isHalfCarry3to4(x, y, cb);
            const carry = matchValue_1;
            const z = isZero(res);
            const resMin = setMin(res, carry);
            const f = new Flags(z, true, half, carry);
            const updatedRegisters = add(a, new BitData(1, [resMin]), regs);
            return [updatedRegisters, f];
        }
        default: {
            toConsole(printf("No 8 bit values detected!"));
            return undefined;
        }
    }
}

export function subCFromMemory(regs, memory) {
    const a = tryFind(new GameBoyRegister(0, []), regs);
    const pc = tryFind(new GameBoyRegister(8, []), regs);
    const cb = getCarryBit(tryFind(new GameBoyRegister(5, []), regs));
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
                    const res = (i - n) - cb;
                    const carry = isCarry7to8(i, n, cb);
                    const half = isHalfCarry3to4(i, n, cb);
                    const z = isZero(res);
                    const f = new Flags(z, true, half, carry);
                    const updatedRegs = add(new GameBoyRegister(8, []), new BitData(0, [adress + 1]), add(new GameBoyRegister(0, []), new BitData(1, [setMin(res, carry)]), regs));
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

export function filterSubC(r1, r2, reg, mem) {
    let matchResult, n;
    switch (r2.tag) {
        case 3: {
            if (r2.fields[0].tag === 2) {
                if (r2.fields[0].fields[0].tag === 3) {
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
            if (r2.fields[0].tag === 1) {
                matchResult = 1;
            }
            else {
                matchResult = 3;
            }
            break;
        }
        case 1: {
            matchResult = 2;
            n = r2.fields[0];
            break;
        }
        default:
            matchResult = 3;
    }
    switch (matchResult) {
        case 0:
            return subCHLpointer(reg, mem);
        case 1:
            return subCFromMemory(reg, mem);
        case 2:
            return subC8bitRegisters(new GameBoyRegister(0, []), n, reg);
        default:
            return undefined;
    }
}

