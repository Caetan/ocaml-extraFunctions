

(***********************************************************************


	Developed by Caetán Tojeiro Carpente (caetantojeiro95@gmail.com)
	
	

Copyleft (C) 2017  Caetán Tojeiro Carpente

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>



************************************************************************)


let suml l =
	let rec aux s = function
		  [] -> 0
		| [h] -> s+h
		| h::t -> aux (s+h) t
	in aux 0 l;;


let maxl l = match l with 
  [] -> raise (Failure "maxl")
| h::[] -> h
| h::t -> let rec aux m = function
			[] -> m
			| h::t -> if (m>h); then aux m t
						else aux h t
		in aux h l;;

let to0from n = 
	let rec aux l i =
		if i<0 then List.rev l
		else aux (i::l) (i-1)
	in aux [] n;;
	
	
let fromto m n = 
	let rec aux l i =
		if i<m then l
		else aux (i::l) (i-1)
	in aux [] n;;

	
let from1to n = 
	let rec aux l i =
		if i<1 then l
		else aux (i::l) (i-1)
	in aux [] n;;

	
let append l1 l2 = match (l1,l2) with
	  [],[] -> []
	| [],_ -> l2
	| _,[] -> l1
	| _,_ -> List.rev_append (List.rev l1) l2;;

let concat l =
	let rec aux l acc = match l with
		[] -> List.rev acc
		| h::t -> aux t (List.rev_append h acc)
	in aux l [];;

let map f l =
	let rec aux f l acc = match l with
		[] -> List.rev acc
		| h::t -> aux f t ((f h)::acc)		
	in aux f l [];;

let power x y =
	let rec innerpower x y acc =
		if y=0 then acc
		else innerpower x (y-1) (x*acc)
	in
	if y >= 0 then innerpower x y 1
	else invalid_arg "power";;

let fib n = 
	let rec innerfib i f a =
		if i=n then f
		else innerfib (i+1) (f+a) f
	in if n >= 0 then innerfib 0 0 1
	else invalid_arg "fib";;

	
let fact n =
	let rec innerfact n acc =
		if n=0 then acc
		else innerfact (n-1) (acc *. float n)
	in
	if n>=0 then innerfact n 1.
	else invalid_arg "fact";;


let incseg l = 
	let rec aux l acc l2 = match l with
		[] -> []
		| [h] -> List.rev ((h+acc)::l2)
		| h::t -> aux t (h+acc) ((h+acc)::l2)
	in aux l 0 [];;


let multicomp l x = 
	let rec aux l x acc = match l with
		  [] -> acc
		| f::t -> aux t x (f acc)
	in
	if l=[] then x
	else aux l x x;;
	

let insert x l =
	let rec aux x l l2 flag = match l,flag with
		_,false -> List.rev_append l2 l
		| [],_ -> List.rev_append  l2 [x]
		| h::t, true -> if x<=h then aux x t (h::x::l2) false
					else aux x t (h::l2) true
	in aux x l [] true;;
	

let rec insert_gen f x l =
	let rec aux f x l l2 flag = match l,flag with
		  [],_ -> List.rev_append l2 [x]
		| _,false -> List.rev_append l2 l
		| h::t, true -> if f x h then aux f x t (h::x::l2) false
					else aux f x t (h::l2) true
	in aux f x l [] true;;
