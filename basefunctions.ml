let rec suml = function
	[] -> 0
	| h::t -> h + suml t;;

		
let rec maxl = function
	[] -> raise (Failure "maxl")
	| h::[] -> h
	| h::t -> max h (maxl t);;

		
		
let rec to0from n =
	if n < 0 then []
	else n::to0from (n-1);;


let rec fromto m n =
	if m > n then []
	else m::fromto (m+1) n;;


let rec from1to n =
	if n < 1 then []
	else from1to (n-1) @ [n];;

	
let append = List.append;;

let concat = List.concat;;

let map = List.map;;


let power x y =
	let rec innerpower x y =
		if y = 0 then 1
		else x * innerpower x (y-1)
	in
	if y >= 0 then innerpower x y
	else invalid_arg "power";;


let fib n = 
	let rec innerfib n =
		if n < 2 then n
		else innerfib (n-1) + innerfib (n-2)
	in
	if n >= 0 then innerfib n
	else invalid_arg "fib";;

	
let fact n =
	let rec innerfact n =
		if n = 0 then 1.
		else float n *. innerfact (n-1)
	in
	if n >= 0 then innerfact n
	else invalid_arg "fact";;


let incseg l = List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;


let rec multicomp l x = match l with
  [] -> x
| f::t -> f (multicomp t x);;


let rec insert x = function
	[] -> [x]
	| h::t -> if x <= h then x::h::t
				else h::insert x t;;


let rec insert_gen f x l = match l with
[] -> [x]
|h::t -> if f x h then x::l
	else h::insert_gen f x t;;