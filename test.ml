let rec sum l = 
	match l with
	| [] -> 0
	| hd :: tl -> hd + sum tl 
;;

sum[1;2;3];;

sum[];;

let rec drop_value l to_drop =
	match l with
	| [] -> []
	| hd :: tl ->
	  let new_tl = drop_value tl to_drop in
	  if hd = to_drop then new_tl else hd :: new_tl
;;

 
let rec drop_zero l =
	match l with
	| [] -> []
	| 0 :: tl -> drop_zero tl
	| hd :: tl -> hd :: drop_zero tl
;;

drop_zero [1;2;0;3];;


#require "core_bench";;
open Core_bench.Std;;
let run_bench tests =
Bench.bench
  ~ascii_table:true
  ~display:Textutils.Ascii_table.Display.column_titles
  tests
;;


(*
 * OCaml Ships with two compilers: the ocamlc bytecode compiler and the ocamlopt native-code compiler.
 * Programs compiled with ocamlc are interpreted by a virtual machine, while programs compiled with
 * ocamlopt are compiled to native machine code to be run on a specific operating system and processor
 * architecture. With ocamlbuild,, targets ending with .byte are build as bytecode executables, 
 * and those ending with .native are built as native code.
 *)

