open Browser
open Register
open Processor
open Fetch
let b = 100

let mutable a = 255

a <- ( a ||| b) //load b into a
a <- a &&& b //mask unnecessary bits


let test1,test2 = 0x0F,0x0F
let test3 = 0x0F0F
let test4 = (bytesToShort (Byte test1) (Byte test2))
let b2 = Byte 255


let testByte = Byte 0xE2

printfn "test negative is %A" (testByte.byteValue |> byteToSigned)
printfn "test negative  2 is %A" (testByte.byteValue)
let div = document.createElement "div"
div.innerHTML <- (test3.ToString())
document.body.appendChild div |> ignore

