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

let lst = [1; 2; 3; 4; 5]
let lst' = [6; 7] @ lst
let rec print_list l =
  match l with
  | [] -> Printf.printf "\n"
  | h :: t -> let 
    _ = Printf.printf "%d " h 
    in print_list t

let () = print_list lst
let () = print_list lst'

let iter_test l =
  let _ = List.iter (fun x -> Printf.printf "%d " x) l in
  Printf.printf "\n"

let () = iter_test lst

let map_test l = 
  let l' = List.map TT.square l in
  print_list l'

let () = map_test lst

let fold_test l =
  let new_sum = List.fold_left (fun acc x -> acc + x) 0 l in
  Printf.printf "sum %d\n" new_sum

let () = fold_test lst

type slist = string list
let _ = 
  let is_empty (x: slist) =
    match x with
    | [] -> true
    | _ -> false
  in
  Printf.printf "is_empty %b\n" (is_empty ["a"; "b"])

type token_type =
  | Null
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | List of token_type list
  | Object of token_type list

let token_to_str (t: token_type) =
  match t with
  | Null -> "Null"
  | Int i -> Printf.sprintf "Int %d" i
  | Float f -> Printf.sprintf "Float %f" f
  | String s -> Printf.sprintf "String %s" s
  | Symbol s -> Printf.sprintf "Symbol %s" s
  | _ -> "Not implemented"

let _ =
  let a: token_type = Int 3 in
  let y : token_type = Float 4.0 in
  let z : token_type = String "5.0" in
  let w : token_type = Symbol "my_symbol" in
  let (lst: token_type list) = [a; y; z; w] in
  let str_list = List.map token_to_str lst in
  let _ = List.iter (fun x -> Printf.printf "%s " x) str_list in
  Printf.printf "\n"


type person = 
{
  name: string;
  age: int;
  height: float;
}

let frankie = {name = "Frankie"; age = 22; height = 166.6}
let print_person p =
  Printf.printf "name: %s, age: %d, height: %f\n" p.name p.age p.height
let _ = print_person frankie

type 'a btree =
  | Leaf
  | Node of 'a btree * 'a * 'a btree

let bt = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Node (Leaf, 4, Leaf)))
let rec print_btree bt =
  match bt with
  | Leaf -> Printf.printf "Leaf\n"
  | Node (l, v, r) -> 
    let _ = print_btree l in
    let _ = Printf.printf "Node %d\n" v in
    print_btree r
let _ = print_btree bt