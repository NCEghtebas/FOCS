(* FoCS Spring 2015

   Homework 7 code

   Name: Chloe Eghtebas

   Email: nassim.eghtebas@students.olin.edu

   Comments: 

 *)



(* 
 *   explode_str : string -> string list
 *      returns the list of characters making up a string
 *      (where each character gets returned as a string)
 *
 *)

let explode_str (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.sub str index 1)::result)
  in
    acc(String.length(str)-1, [])




(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type symbol = string

type 'a tm = { tm_states : 'a list;
               tm_input_alph : symbol list;
	       tm_tape_alph : symbol list;
	       tm_leftmost : symbol;
	       tm_blank : symbol;
	       tm_delta : ('a * symbol * 'a * symbol * direction) list;
	       tm_start : 'a;
	       tm_accept : 'a;
	       tm_reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 *  and return a value
 * 
 *)

let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value


(* QUESTION 1 *)

let starting_config m w = ([] , m.tm_start, [m.tm_leftmost]@(explode_str w))

let accepting_config m c = (match c with
							| (a, b, c) -> (m.tm_accept = b) )

let rejecting_config m c = (match c with 
							| (a, b, c) -> (m.tm_reject = b) )

let halting_config m c = (accepting_config m c) || (rejecting_config m c)

let get_nextdelta m q ch =  List.find (fun (a1, a2, a3, a4, dir) -> ((a1 = q) && (a2 = ch)) ) m.tm_delta

let step_config m c = match c with
					| (a, b, d) -> (match get_nextdelta m b (List.hd d) with 
									| (q1, ch1, q2, ch2, dir) -> (if dir = Right then 
																	( a@[ch2] , q2 , (List.tl d) )
																  else 
																  	( (List.rev (List.tl (List.rev a))), q2 , ( (List.hd (List.rev a))::ch2::(List.tl d)) ) ) )

let rec helper_run m c = let z = print_config m c 0
     						in (if halting_config m c then
									accepting_config m c 
								else 
									match c with
									| (a, b, []) -> helper_run m (step_config m (a, b, [m.tm_blank])) 
									| (a, b, d) -> helper_run m (step_config m c) )


let run m w = let starting = starting_config m w in (helper_run m starting)





(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { tm_states = ["start"; "q1"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "acc", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "_", "acc", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbn = { tm_states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ ("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "q2", "_", Right);
		        ("start", "X", "rej", "X", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "_", "q2", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);
		        ("q2", ">", "q3", ">", Right);
		        ("q2", "a", "q2", "a", Left);
		        ("q2", "b", "q2", "b", Left);
		        ("q2", "X", "q2", "X", Left);
		        ("q2", "_", "q2", "_", Left);
		        ("q3", "X", "q3", "X", Right);
		        ("q3", "_", "acc", "_", Right);
		        ("q3", "a", "q4", "X", Right);
		        ("q3", "b", "rej", "b", Right);
		        ("q3", ">", "rej", ">", Right);
		        ("q4", "a", "q4", "a", Right);
		        ("q4", "X", "q4", "X", Right);
		        ("q4", "b", "q2", "X", Right);
		        ("q4", "a", "rej", "a", Right);
		        ("q4", ">", "rej", ">", Right);
		        ("q4", "_", "rej", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbncn = { tm_states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       tm_input_alph = ["a";"b";"c"];
	       tm_tape_alph = ["a";"b";"c";"X";"_";">"];
	       tm_blank = "_";
	       tm_leftmost = ">";
	       tm_start = "start";
	       tm_accept = "acc";
	       tm_reject = "rej";
	       tm_delta = [ ("start", "a", "start", "a", Right);
     	          ("start", "b", "q1", "b", Right);
		  ("start", "c", "q6", "c", Right);
		  ("start", ">", "start", ">", Right);
		  ("start", "_", "q2", "_", Right);
		  ("start", "X", "rej", "X", Right);
		  ("q1", "b", "q1", "b", Right);
		  ("q1", "c", "q6", "c", Right);
		  ("q1", "_", "q2", "_", Right);
		  ("q1", "a", "rej", "a", Right);
		  ("q1", ">", "rej", ">", Right);
		  ("q1", "X", "rej", "X", Right);
		  ("q2", ">", "q3", ">", Right);
		  ("q2", "a", "q2", "a", Left);
		  ("q2", "b", "q2", "b", Left);
		  ("q2", "c", "q2", "c", Left);
		  ("q2", "_", "q2", "_", Left);
		  ("q2", "X", "q2", "X", Left);
		  ("q3", "X", "q3", "X", Right);
		  ("q3", "_", "acc", "_", Right);
		  ("q3", "a", "q4", "X", Right);
		  ("q3", "b", "rej", "b", Right);
		  ("q3", "c", "rej", "c", Right);
		  ("q3", ">", "rej", ">", Right);
		  ("q4", "a", "q4", "a", Right);
		  ("q4", "X", "q4", "X", Right);
		  ("q4", "b", "q5", "X", Right);
		  ("q4", "c", "rej", "c", Right);
		  ("q4", "_", "rej", "_", Right);
		  ("q4", ">", "rej", ">", Right);
		  ("q5", "b", "q5", "b", Right);
		  ("q5", "X", "q5", "X", Right);
		  ("q5", "c", "q2", "X", Right);
		  ("q5", "a", "rej", "a", Right);
		  ("q5", "_", "rej", "_", Right);
		  ("q5", ">", "rej", ">", Right);
		  ("q6", "c", "q6", "c", Right);
		  ("q6", "_", "q2", "_", Right);
		  ("q6", "a", "rej", "a", Right);
		  ("q6", "b", "rej", "b", Right);
		  ("q6", ">", "rej", ">", Right);
		  ("q6", "X", "rej", "X", Right);
		  ("acc", "a", "acc", "a", Right);
		  ("acc", "b", "acc", "b", Right);
		  ("acc", "c", "acc", "c", Right);
		  ("acc", ">", "acc", ">", Right);
		  ("acc", "X", "acc", "X", Right);
		  ("acc", "_", "acc", "_", Right);
		  ("rej", "a", "rej", "a", Right);
		  ("rej", "b", "rej", "b", Right);
		  ("rej", "c", "rej", "c", Right);
		  ("rej", ">", "rej", ">", Right);
		  ("rej", "X", "rej", "X", Right);
		  ("rej", "_", "rej", "_", Right)] }




(* QUESTION 2 *)


let question2a = { tm_states = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "acc"; "rej"];
		   tm_input_alph = ["a"; "b"];
		   tm_tape_alph = ["a";"b";"x";"_";">"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [ ("A", ">", "A", ">", Right);
		   				("A", "a", "B", "x", Right);
		   				("A", "b", "E", "b", Right);
		   				("A", "_", "acc", "_", Right);
		   				("B", "a", "B", "a", Right);
		   				("B", "b", "C", "b", Right);
		   				("B", "_", "G", "_", Left);
		   				("C", "b", "C", "b", Right);
		   				("C", "x", "C", "x", Right);
		   				("C", "a", "D", "x", Right);
		   				("C", "_", "rej", "_", Right);
		   				("D", "a", "D", "a", Left);
		   				("D", "b", "D", "b", Left);
		   				("D", "x", "D", "x", Left);
		   				("D", ">", "I", ">", Right);
		   				("D", "_", "rej", "_", Right);
		   				("I", "x", "I", "x", Right);
		   				("I", "a", "B", "x", Right);
		   				("I", "b", "J", "x", Right);
		   				("J", "x", "J", "x", Right);
		   				("J", "a", "rej", "a", Right);
		   				("J", "b", "K", "x", Left);
		   				("J", "_", "rej", "_", Left);
		   				("K", "b", "K", "b", Left);
		   				("K", "x", "K", "x", Left);
		   				("K", ">", "L", ">", Right);
		   				("L", "x", "L", "x", Right);
		   				("L", "_", "acc", "_", Right);
		   				("L", "b", "J", "x", Right);
		   				("L", "a", "rej", "a", Left);
		   				("E", "_", "rej", "_", Right);
		   				("E", "b", "F", "b", Right);
		   				("E", "a", "rej", "a", Right);
		   				("F", "_", "acc", "_", Right);
		   				("F", "b", "E", "b", Right);
		   				("G", "a", "H", "a", Left);
		   				("G", "x", "H", "x", Left);
		   				("G", ">", "acc", ">", Right);
		   				("H", "a", "G", "a", Left);
		   				("H", "x", "G", "x", Left);
		   				("H", ">", "rej", ">", Right);
		   				
		   ];
		   tm_start = "A";
		   tm_accept = "acc";
		   tm_reject = "rej" }


let question2b = { tm_states = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "acc"; "rej"];
		   tm_input_alph = ["a";"b"];
		   tm_tape_alph = ["a";"b";"a1";"b1";"_";">"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [ ("A", ">", "B", ">", Right);
		   				("B", "a", "C", "a1", Right);
		   				("B", "b", "H", "b1", Right);
		   				("C", "a", "C", "a", Right);
		   				("C", "b", "D", "b1", Right);
		   				("C", "x", "F", "x", Left);
		   				("C", "_", "acc", "_", Right);
		   				("D", "b", "D", "b", Right);
		   				("D", "x", "D", "x", Right);
		   				("D", "a", "E", "x", Left);
		   				("D", "_", "rej", "_", Right);
		   				("E", "x", "E", "x", Left);
		   				("E", "b", "E", "b", Left);
		   				("E", "b1", "C", "b1", Right);
		   				("F", "a", "F", "a", Left);
		   				("F", "b1", "F", "b", Left);
		   				("F", "a1", "B", "a1", Right);
		   				("G", "_", "acc", "_", Right);
		   				("G", "a", "rej", "a", Right);
		   				("G", "b", "rej", "b", Right);
		   				("G", "x", "G", "x", Right);
		   				("H", "b", "H", "b", Right);
		   				("H", "x", "G", "x", Right);
		   				("H", "a", "rej", "a", Right);
		   				("H", "_", "acc", "_", Right)
		   				];
		   tm_start = "A";
		   tm_accept = "acc";
		   tm_reject = "rej" }



(* QUESTION 3 *)


let binary_sum = { tm_states = [];
		   tm_input_alph = [];
		   tm_tape_alph = [];
		   tm_leftmost = "";
		   tm_blank = "";
		   tm_delta = [];
		   tm_start = "";
		   tm_accept = "";
		   tm_reject = "" }


