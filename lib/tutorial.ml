let hello = "HELLO"

let v = hello ^ " World!!!"

let x = 50
let y = -3

let square x = x * x

let rec factorial n = 
  if n = 0 
    then 1 
    else n * factorial (n - 1)


let classify x =
  match x with
  | 0 -> "Zero"
  | x when x < 0 -> "Negative"
  | x when x > 0 -> "Positive"
  | _ -> "Unknown"

