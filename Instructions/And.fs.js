import { add, tryFind } from "../fable_modules/fable-library-js.4.17.0/Map.js";
import { BitData, regsToVirtual, GameBoyRegister } from "../Registers.fs.js";
import { printf, toConsole } from "../fable_modules/fable-library-js.4.17.0/String.js";
import { Flags } from "../Flags.fs.js";

export function setMinMax(n) {
    const matchValue = n < 0;
    const matchValue_1 = n > 255;
    let matchResult;
    if (matchValue) {
        if (matchValue_1) {
            matchResult = 2;
        }
        else {
            matchResult = 0;
        }
    }
    else if (matchValue_1) {
        matchResult = 1;
    }
    else {
        matchResult = 2;
    }
    switch (matchResult) {
        case 0:
            return 0;
        case 1:
            return 255;
        default:
            return n;
    }
}

export function andHLpointer(regs, mem) {
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
                const res = b & dataHL;
                const f = new Flags(res === 0, false, true, false);
                const ur = add(new GameBoyRegister(0, []), new BitData(1, [setMinMax(res)]), regs);
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

export function and8bitRegisters(a, b, regs) {
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
            const res = x & y;
            const f = new Flags(res === 0, false, true, false);
            const updatedRegisters = add(a, new BitData(1, [setMinMax(res)]), regs);
            return [updatedRegisters, f];
        }
        default: {
            toConsole(printf("No 8 bit values detected!"));
            return undefined;
        }
    }
}

export function andFromMem(regs, memory) {
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
                    toConsole(printf("N8 is %A"))(n);
                    toConsole(printf("REG is %A"))(i);
                    const res = i & n;
                    const f = new Flags(res === 0, false, true, false);
                    const updatedRegs = add(new GameBoyRegister(8, []), new BitData(0, [adress + 1]), add(new GameBoyRegister(0, []), new BitData(1, [setMinMax(res)]), regs));
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

export function filterAnd(r1, r2, reg, mem) {
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
            return andHLpointer(reg, mem);
        case 1:
            return andFromMem(reg, mem);
        case 2:
            return and8bitRegisters(new GameBoyRegister(0, []), n, reg);
        default:
            return undefined;
    }
}

