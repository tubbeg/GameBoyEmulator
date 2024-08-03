open Browser
open Register


let x = Byte 256


let div = document.createElement "div"
div.innerHTML <- (x.byteValue.ToString())
document.body.appendChild div |> ignore

