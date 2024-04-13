let () = Printf.printf "%s\n" Tutorial.v
let () = Printf.printf "%d -> %d\n" Tutorial.x (Tutorial.square Tutorial.x)
let () = Printf.printf "%d -> %d\n" 3 (Tutorial.factorial 3)
let () = Printf.printf "%d -> %s\n" Tutorial.y (Tutorial.classify Tutorial.y)
let () = Printf.printf "%d\n" (Tutorial.sum Tutorial.il)

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)


let () = Printf.printf "gcd(%d, %d) = %d\n" 60 40 (gcd 60 40)
let () = 
  let open Tutorial in
  Printf.printf "tut %d -> %d\n" 3 (square 3)

module TT = Tutorial
let () = Printf.printf "TT %d -> %d\n" 4 (TT.square 4)
let () = Printf.printf "TT %d -> %d\n" 5 (TT.square 5)

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let print p f x =
  Printf.printf p x (f x)

let () = print "is digit %c -> %b\n" is_digit 'a'
let () = print "is digit %c -> %b\n" is_digit '5'

