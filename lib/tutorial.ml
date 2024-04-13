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

let il = [1; 2; 3; 4; 5]
let rec sum l =
  match l with
  | [] -> 0
  | h::t -> h + sum t



let test i f =
  "i: " ^ string_of_int i ^ ", f: " ^ string_of_int (f i)
