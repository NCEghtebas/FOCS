(* FoCS Spring 2015

   Homework 8 code


   Name: Chloe Eghtebas

   Email: nassim.eghtebas@students.olin.edu

   Comments:  Next steps for problem 3 granted more time: Take one tm2 delta and split it apart into 2 
   tm2 deltas add a state inbetween. This state inbetween cycles through everything until it 
   arrives at the next tape head. Then it preforms the second delta from spliting tm2 delta 
   operation. The two tape tm on one tape is simulated but much slower because you only have 
   one tape head doing the work of two and twice as many deltas. 

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
 *   Type for deterministic Turing machines
 *
 *   Parameterized by type for states
 *)

type direction = Left | Right | Stay

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
 *   Run a Turing machine on an input word
 *)

let run m w = 

  let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value  in

  let starting_config = 
    ([], m.tm_start, m.tm_leftmost::(explode_str w))  in

  let accepting_config m (u,q,v) = (q=m.tm_accept)  in

  let rejecting_config m (u,q,v) = (q=m.tm_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v) = 

    let rec find_match (q,a) delta = 
      match delta with
        [] -> failwith "No transition defined!"
      | ((q_,sym_,p_,rewrite_,dir_)::_) when q=q_ && a=sym_ -> (p_,rewrite_,dir_)
      | _::delta' -> find_match (q,a) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v)) 
      then (u,q,v)
    else let (a,v') = match v with
                      | [] -> (m.tm_blank,[])
                      | a::v' -> (a,v')  in
         let (q',b,dir) = find_match (q,a) m.tm_delta  in
         match dir with
         | Left -> let (u',c) = last u in (u',q',c::b::v')
         | Right -> (u@[b],q',v')
         | Stay -> (u,q',b::v')  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config



(* 
 *
 * QUESTION 1
 *
 *)

let singledelta s a f = 
  match f (s,a) with
  | (b,c,d)-> (s, a, b, c, d)

let alphdelta s al f = List.map (fun x -> singledelta s x f) al

let make_delta states alph f = List.flatten (List.map (fun x -> alphdelta x alph f) states)
  
let transform_states t states = List.map (fun x -> t x) states

let transform_delta t delta = List.map (fun (sa, b, sc, d, e) -> (t sa, b, t sc , d, e)) delta

let transform t tm = {tm_states = (transform_states t tm.tm_states);
                      tm_input_alph = tm.tm_input_alph;
                      tm_tape_alph = tm.tm_tape_alph;
                      tm_blank = tm.tm_blank;
                      tm_leftmost = tm.tm_leftmost;
                      tm_delta = (transform_delta t (tm.tm_delta) );
                      tm_start = (t tm.tm_start);
                      tm_accept = (t tm.tm_accept);
                      tm_reject = (t tm.tm_reject)
                      }




(* 
 * Some sample deterministic Turing machines
 *
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)


let anbn = 
  let states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"]  in
  let tape_alph = ["a";"b";"X";"_";">"]  in
  { tm_states = states;
    tm_input_alph = ["a";"b"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q2", "X", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right)) }


let anbncn = 
  let states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"] in
  let tape_alph = ["a";"b";"c";"X";"_";">"] in
  { tm_states = states;
    tm_input_alph = ["a";"b";"c"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", "c") -> ("q6", "c", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "c") -> ("q6", "c", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q5", "X", Right)
        | ("q5", "b") -> ("q5", "b", Right)
        | ("q5", "X") -> ("q5", "X", Right)
        | ("q5", "c") -> ("q2", "X", Right)
        | ("q6", "c") -> ("q6", "c", Right)
        | ("q6", "_") -> ("q2", "_", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right) )}


(* see write up *)

let evenOddSequence = 
  let delta (p,a) = 
    match p,a with
    | "even", ">" -> ("even", ">", Right)
    | "even", "0" -> ("odd", "0", Right)
    | "even", "1" -> ("even", "1", Right)
    | "even", "_" -> ("q1/0", "_", Left)
          
    | "odd", "0" -> ("even", "0", Right)
    | "odd", "1" -> ("odd", "1", Right)
    | "odd", "_" -> ("q1/1", "_", Left)
          
    | "q1/0", ">" -> ("acc", ">", Right)
    | "q1/0", "0" -> ("q2/0", "0", Left)
    | "q1/0", "1" -> ("q1/0", "1", Left)
          
    | "q2/0", ">" -> ("acc", ">", Right)
    | "q2/0", "0" -> ("q3/0", "0", Left)
    | "q2/0", "1" -> ("q1/0", "1", Left)
          
    | "q3/0", ">" -> ("acc", ">", Right)
    | "q3/0", "1" -> ("q1/0", "1", Left)
          
    | "q1/1", ">" -> ("acc", ">", Right)
    | "q1/1", "0" -> ("q1/1", "0", Left)
    | "q1/1", "1" -> ("q2/1", "1", Left)
          
    | "q2/1", ">" -> ("acc", ">", Right)
    | "q2/1", "0" -> ("q1/1", "0", Left)
    | "q2/1", "1" -> ("q3/1", "1", Left)
          
    | "q3/1", ">" -> ("acc", ">", Right)
    | "q3/1", "0" -> ("q1/1", "0", Left)
          
    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  let states = [ "even";"odd";"q1/0";"q2/0";"q3/0";"q1/1";"q2/1";"q3/1";
                 "acc";"rej"]  in
  let alph = ["0";"1";"_";">"]  in
  { tm_states = states ;
    tm_input_alph = ["0";"1"];
    tm_tape_alph = alph;
    tm_leftmost = ">";
    tm_blank = "_";
    tm_delta = make_delta states alph delta;
    tm_start = "even";
    tm_accept = "acc";
    tm_reject = "rej" }



(* A version of the same Turing machine but with
   structured states 
 *)

type even_odd_sequence_state = 
  | Simple of string
  | BitTag of string * string

let evenOddSequence_struct = 
  let delta (p,a) = 
    match p,a with
    | Simple("even"), ">" -> (Simple("even"), ">", Right)
    | Simple("even"), "0" -> (Simple("odd"), "0", Right)
    | Simple("even"), "1" -> (Simple("even"), "1", Right)
    | Simple("even"), "_" -> (BitTag("q1", "0"), "_", Left)
          
    | Simple("odd"), "0" -> (Simple("even"), "0", Right)
    | Simple("odd"), "1" -> (Simple("odd"), "1", Right)
    | Simple("odd"), "_" -> (BitTag("q1", "1"), "_", Left)
          
    | BitTag("q1", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q1", t), sym when t = sym -> (BitTag("q2", t), sym, Left)
    | BitTag("q1", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q2", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q2", t), sym when t = sym -> (BitTag("q3", t), sym, Left)
    | BitTag("q2", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q3", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q3", t), sym when t <> sym -> (BitTag("q1", t), sym, Left)
          
    | Simple("acc"), sym -> (Simple("acc"), sym, Right)
    | _, sym -> (Simple("rej"), sym, Right)  in
  let states = [ Simple("even"); Simple("odd"); Simple("acc"); Simple("rej");
                 BitTag("q1", "0"); BitTag("q2", "0"); BitTag("q3", "0"); 
                 BitTag("q1", "1"); BitTag("q2", "1"); BitTag("q3", "1") ]  in
  let alph = ["0"; "1"; ">"; "_"]  in
  let string_of st = 
    match st with
    | Simple(s) -> s
    | BitTag(s,b) -> s^"|"^b  in
  transform string_of
    { tm_states = states;
      tm_input_alph = ["0"; "1"];
      tm_tape_alph = alph;
      tm_leftmost = ">";
      tm_blank = "_";
      tm_delta = make_delta states alph delta;
      tm_start = Simple("even");
      tm_accept = Simple("acc");
      tm_reject = Simple("rej") }




(* 
 *
 * QUESTION 2
 *
 *)


(* some helper definitions *)

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let isDigit c = List.mem c digits

let rejectif nstate sym rej=
  if isDigit sym then
    (nstate, sym, Right)
  else
    (rej, sym, Right)

let rejectifL nstate sym rej=
  if isDigit sym then
    (nstate, sym, Left)
  else
    (rej, sym, Left)

let check nstate sym sym2 rl rej=
  if sym = sym2 then 
    (nstate, "x", rl)
  else
    (rej, sym2 , rl)

type triple_sequence_state = 
  | Simple of string
  | DigTag of string * string

let transimple xs=
  List.map (fun x->Simple(x)) xs 

let trippple =
    let delta (s,a)=
     match s,a with
      | Simple("start"), ">" -> (Simple("A"), ">", Right)
      | Simple("A"), sym -> rejectif (Simple("B")) sym (Simple("rej"))
      | Simple("B"), "#" -> (Simple("C"), "#", Right)
      | Simple("B"), sym -> rejectif (Simple("B")) sym (Simple("rej"))
      | Simple("C"), sym -> rejectif (Simple("D")) sym (Simple("rej"))
      | Simple("D"), "#" -> (Simple("E"), "#", Right)
      | Simple("D"), sym -> rejectif (Simple("D")) sym (Simple("rej"))
      | Simple("E"), sym -> rejectif (Simple("F")) sym (Simple("rej"))
      | Simple("F"), "_" -> (Simple("G"), "_", Left)
      | Simple("F"), sym -> rejectif (Simple("F")) sym (Simple("rej"))
      | Simple("G"), "#" -> (Simple("G"), "#", Left)
      | Simple("G"), ">" -> (Simple("H"), ">", Right)
      | Simple("G"), sym -> rejectifL (Simple("G")) sym (Simple("rej"))
      | Simple("H"), "#" -> (Simple("O"), "#", Right)
      | Simple("H"), "x" -> (Simple("H"), "x", Right)
      | Simple("H"), sym -> (DigTag("I", sym), "x", Right)
      | DigTag("I", sym2), "#" -> (DigTag("J", sym2), "#", Right)
      | DigTag("I", sym2), sym -> (DigTag("I", sym2), sym, Right)
      | DigTag("J", sym2), "x" -> (DigTag("J", sym2), "x", Right)
      | DigTag("J", sym2), sym -> check (DigTag("K", sym2)) sym sym2 Right (Simple("rej"))
      | DigTag("K", sym2), "#" -> (DigTag("L", sym2), "#", Right)
      | DigTag("K", sym2), sym -> (DigTag("K", sym2), sym, Right)
      | DigTag("L", sym2), "x" -> (DigTag("L", sym2), "x", Right)
      | DigTag("L", sym2), sym -> check (Simple("N")) sym sym2 Left (Simple("rej"))
      | Simple("N"), ">" -> (Simple("H"), ">", Right)
      | Simple("N"), sym -> (Simple("N"), sym, Left)
      | Simple("O"), "x" -> (Simple("O"), "x", Right)
      | Simple("O"), "#" -> (Simple("O"), "#", Right)
      | Simple("O"), "_" -> (Simple("acc"), "_", Right)
      | Simple( _ ), sym -> (Simple("rej"), sym , Right)
      | DigTag(_,_), sym ->(Simple("rej"), sym , Right)in
  let states = [Simple("A"); Simple("B"); Simple("C"); Simple("D"); Simple("E"); 
                Simple("F"); Simple("G"); Simple("H"); 
                DigTag("I", "0"); DigTag("I", "1"); DigTag("I", "2"); DigTag("I", "3"); DigTag("I", "4"); DigTag("I", "5"); DigTag("I", "6"); DigTag("I", "7"); DigTag("I", "8"); DigTag("I", "9");
                DigTag("J", "0"); DigTag("J", "1"); DigTag("J", "2"); DigTag("J", "3"); DigTag("J", "4"); DigTag("J", "5"); DigTag("J", "6"); DigTag("J", "7"); DigTag("J", "8"); DigTag("J", "9");                 
                DigTag("K", "0"); DigTag("K", "1"); DigTag("K", "2"); DigTag("K", "3"); DigTag("K", "4"); DigTag("K", "5"); DigTag("K", "6"); DigTag("K", "7"); DigTag("K", "8"); DigTag("K", "9");
                DigTag("L", "0"); DigTag("L", "1"); DigTag("L", "2"); DigTag("L", "3"); DigTag("L", "4"); DigTag("L", "5"); DigTag("L", "6"); DigTag("L", "7"); DigTag("L", "8"); DigTag("L", "9");
                Simple("N"); Simple("O"); Simple("start" ); Simple("acc"); Simple("rej")]  in

  let alph = digits@["#"; ">"; "_"; "x"]  in
   { tm_states = states;
     tm_input_alph = "#"::digits;
     tm_tape_alph = alph;
     tm_leftmost = ">";
     tm_blank = "_";
     tm_delta = (make_delta states alph delta);
     tm_start = Simple("start");
     tm_accept = Simple("acc");
     tm_reject = Simple("rej")}

let toStringTriple x =
  match x with
    | Simple(x)->""^x^""
    | DigTag(x,y)->x^"|"^y

let triple = transform toStringTriple trippple 

(* 
 *
 * QUESTION 3
 *
 *)



(*
 * Two-tape Turing machines
 *
 *)

type 'a tm2 = { tm2_states : 'a list;
                tm2_input_alph : symbol list;
                tm2_tape_alph : symbol list;
                tm2_leftmost : symbol;
                tm2_blank : symbol;
                tm2_delta : ('a * symbol * symbol * 'a * symbol * symbol * direction * direction) list;
                tm2_start : 'a;
                tm2_accept : 'a;
                tm2_reject : 'a }


(* 
 *     A sample two-tape TM that decides {u#u | u a string}
 *)

let pair = 
  let foreach l f = List.flatten (List.map f l)  in
  let states = [ "1";"2";"3";"4";"5";"6";"acc";"rej" ]  in
  let alph = ["0";"1";"#";"_";">"]  in
  let default = foreach states 
                  (fun q -> foreach alph 
                              (fun sym1 -> foreach alph
                                             (fun sym2 -> [(q,sym1,sym2,"rej",sym1,sym2,Stay,Stay)])))  in
  { tm2_states = states;
    tm2_input_alph = [ "0";"1";"#" ];
    tm2_tape_alph = alph;
    tm2_leftmost = ">";
    tm2_blank = "_";
    tm2_delta = [ ("1",">",">","2",">",">",Right,Right);
                  ("2","0","_","2","0","_",Right,Stay);
                  ("2","1","_","2","1","_",Right,Stay);
                  ("2","#","_","3","_","_",Right,Stay);
                  ("3","0","_","3","_","0",Right,Right);
                  ("3","1","_","3","_","1",Right,Right);
                  ("3","_","_","4","_","_",Left,Stay);
                  ("4","_","_","4","_","_",Left,Stay);
                  ("4","0","_","4","0","_",Left,Stay);
                  ("4","1","_","4","1","_",Left,Stay);
                  ("4",">","_","5",">","_",Stay,Stay);
                  ("5",">","_","5",">","_",Stay,Left);
                  ("5",">","0","5",">","0",Stay,Left);
                  ("5",">","1","5",">","1",Stay,Left);
                  ("5",">",">","6",">",">",Right,Right);
                  ("6","0","0","6","0","0",Right,Right);
                  ("6","1","1","6","1","1",Right,Right);
                  ("6","_","_","acc","_","_",Stay,Stay)
                ] @ default;
    tm2_start = "1";
    tm2_accept = "acc";
    tm2_reject = "rej" }



(* 
 *   Some code to run a two-tape TM on an input word
 *)

let run2 m w = 

  let print_config m (u,q,v,u2,v2) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_string " | "  in
    let _ = print_syms u2  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v2  in
    let _ = print_newline ()  in
    value   in

  let accepting_config m (u,q,v,_,_) = (q=m.tm2_accept)  in

  let rejecting_config m (u,q,v,_,_) = (q=m.tm2_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v,u2,v2) = 

    let rec find_match (q,a,b) delta = 
      match delta with
        [] -> failwith "No transition defined!"
      | ((q_,sym1_,sym2_,p_,rewrite1_,rewrite2_,dir1_,dir2_)::_) 
              when q=q_ && a=sym1_ && b=sym2_ -> (p_,rewrite1_,rewrite2_,dir1_,dir2_)
      | _::delta' -> find_match (q,a,b) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v,u2,v2)) 
      then (u,q,v,u2,v2)
    else let (a,v') = match v with
                      | [] -> (m.tm2_blank,[])
                      | a::v' -> (a,v')  in
         let (a2,v2') = match v2 with
                      | [] -> (m.tm2_blank,[])
                      | a2::v2' -> (a2,v2')  in
         let (q',b,b2,dir,dir2) = find_match (q,a,a2) m.tm2_delta  in
         match dir,dir2 with
         | Left,Left -> let (u',c) = last u in 
                        let (u2',c2) = last u2 in (u',q',c::b::v',u2',c2::b2::v2')
         | Left,Right -> let (u',c) = last u in (u',q',c::b::v',u2@[b2],v2')
         | Left,Stay -> let (u',c) = last u in (u',q',c::b::v',u2,b2::v2')
         | Right,Left -> let (u2',c2) = last u2 in (u@[b],q',v',u2',c2::b2::v2')
         | Right,Right -> (u@[b],q',v',u2@[b2],v2')
         | Right,Stay -> (u@[b],q',v',u2,b2::v2')
         | Stay,Left -> let (u2',c2) = last u2 in (u,q',b::v',u2',c2::b2::v2')
         | Stay,Right -> (u,q',b::v',u2@[b2],v2')
         | Stay,Stay -> (u,q',b::v',u2,b2::v2')   in

  let starting_config = 
    ([], m.tm2_start, m.tm2_leftmost::(explode_str w),[],[m.tm2_leftmost])  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config

type tm2_state = 
  | S of string
  | Mem of string * string

let trans_delta (s,a) =
   match s,a with
    | S("start"), ">" -> (S("1"), ">", Right)
    | S("1"), "0" -> (S("1"), "0", Right)
    | S("1"), "1" -> (S("1"), "1", Right)
    | S("1"), "#" -> (S("1"), "#", Right)
    | S("1"), "_" -> (S("2"), "_", Left)
    | S("2"), ">" -> (S("4"), ">", Right)
    | S("2"), sym -> (Mem("2", sym), "_", Right)
    | Mem("2", sym2), "_" -> (S("3"), sym2, Left)
    | S("3"), sym -> (S("2"), sym, Left)
    | S("4"), "_" -> (S("5"), "^*>", Right)
    | S("5"), "0" -> (S("5"), "0", Right)
    | S("5"), "1" -> (S("5"), "1", Right)
    | S("5"), "#" -> (S("5"), "#", Right)
    | S("5"), "_" -> (S("6"), "|", Right)
    | S("6"), "_" -> (S("7"), "^*>", Left)
    | S("7"), ">" -> (S("start_sim"), ">" , Stay)
    | S("7"), sym -> if sym = "^*>" then
                      (S("7"), "^*>", Left)
                    else
                      (S("7"), "*"^sym, Left)
    | S(_), sym -> (S("rej"), sym, Right)
    | Mem(_, _), sym -> (S("rej"), sym, Right)

let trans_tape t = (List.map (fun y -> "^"^y)  ((List.map (fun x -> "*"^x) t)@t ) )@["|"]@t

let toStringSim2 x =
  match x with
    | S(x)->""^x^""
    | Mem(x,y)->x^"|"^y

let simulate2 tm2 =
  let states = [S("start") ; S("1"); S("2"); 
                Mem("2", "0"); Mem("2", "1"); Mem("2", "#"); Mem("2", "_");
                S("3"); S("4"); S("5"); S("6"); S("7"); S("rej"); S("start_sim")] in
  let alph = (trans_tape tm2.tm2_tape_alph) in
  (transform toStringSim2 
    {tm_states = states;
     tm_input_alph = tm2.tm2_input_alph;
     tm_tape_alph = alph;
     tm_blank = tm2.tm2_blank;
     tm_leftmost = tm2.tm2_leftmost;
     tm_delta = make_delta states alph trans_delta ;
     tm_start = S("start");
     tm_accept = S("start_sim");
     tm_reject = S("rej")
  (* would eventualy replace with what is below:   
     tm_start = tm2.tm2_start;
     tm_accept = tm2.tm2_accept;
     tm_reject = tm2.tm2_reject *)
    } 
  )


   
