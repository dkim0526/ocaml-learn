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




(*
	One of OCaml's betst features is its concise and expressive system for declaring new data types, and records are a key 
	elemtns of that system. 
	A record represents a collection of values stored together as one, where each component is identified by a differnet
	field name. The basic syntax for a record type declaration is as follows:
		type <record-name> =
			{ <field> : <type> ;
			<field> : <type> ;
			...
			}

		Note that record field names must start with a lowercase letter.


*)

#require "core_extended";; 
open Core_extended.Std;; 
let my_host =
	let sh = Shell.sh_one_exn in
	{ hostname = sh"hostname"; 
	  os_name  = sh "uname -s"; 
	  cpu_arch = sh "uname -p";
      timestamp  = Time.now ();
    };;