(* FoCS Spring 2015

   Homework 9 code


   Name: Chloe Eghtebas

   Email: nassim.eghtebas@students.olin.edu

   Comments:

 *)


  
(* The underlying implementation for streams 
 *
 * You don't need to know anything about this code -- you will
 * use functions fby, head, and tail described below instead.
 *
 * But if you're curious, a stream is a pair of an element and a 
 * "promise" to compute the rest of the stream.
 * 
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 *)
  
module AbsStream : 
    sig
      type 'a stream 
      val mk : 'a -> (unit -> 'a stream) -> 'a stream 
      val unmk1 : 'a stream -> 'a 
      val unmk2 : 'a stream -> 'a stream 
    end = 
  struct
    
    type 'a stream = R of 'a * (unit -> 'a stream)
	  
    let memoize f = 
      let memoized = ref None in
      let new_f () = 
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f
	
    let mk h t = R (h, memoize t) 
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()
  end


(*
 * These are the stream functions you will want to use
 *
 *)

type 'a stream = 'a AbsStream.stream
let head : 'a stream -> 'a = AbsStream.unmk1
let tail : 'a stream -> 'a stream = AbsStream.unmk2
let fby : 'a -> (unit -> 'a stream) -> 'a stream = AbsStream.mk






(* 
 * list n s 
 * nth n s
 * list2 n m s
 *
 *
 * list : returns the list of the first 'n' elements of 's'
 * nth : returns the 'n'th element of stream 's'
 * list2 : returns the list of lists of the first 'm' elements
 *           of each of the first 'n' streams of 's'
 *
 *)


let rec list n s = 
  if n <= 0 then []
  else (head s) :: (list (n-1) (tail s))

let rec list2 n m s = 
  List.map (list m) (list n s)

let rec nth n s = 
  if n <= 0 then head s
  else nth (n - 1) (tail s)


(* 
 * const k : returns the constant stream of 'k's
 * from k : returns the stream of integers starting at 'k'
 * 
 *)

let rec const k = fby k (fun () -> const k)

let rec from k = fby k (fun () -> from (k+1))

let nats = from 0

(*
 * map f s : returns the stream obtained by applying 'f' 
 *             to every element of 's' 
 * filter p s : returns the stream of elements of 's' for which 'p' is true
 *
 *)

let rec map f s = fby (f (head s)) (fun () -> (map f (tail s)))

let rec map2 f s1 s2=  fby (f (head s1) (head s2) ) (fun () -> (map2 f (tail s1) (tail s2)  ) )

let plus1 s = map (fun x -> x + 1) s

let evens = map (fun x -> 2 * x) nats

let odds = plus1 evens

let squares = map (fun x -> x * x) nats

let rec filter p s = 
  if (p (head s)) 
  then fby (head s) (fun () -> filter p (tail s))
  else filter p (tail s)

let even s = filter (fun x -> (x mod 2 = 0)) s



(*
 * The Sieve of Eratosthenes
 *
 *)


let not_divisible_by n k = not (k mod n = 0)

let rec sieve s = 
   fby (head s) 
       (fun () -> sieve (filter (not_divisible_by (head s)) (tail s)))

let primes = sieve (from 2)



(* 
 * QUESTION 1 
 * 
 *)

let scale n s = map (fun x -> (x * n ) ) s

let rec zip s1 s2 =  map2 (fun x y -> (x,y)) s1 s2

let add s1 s2 = map2 (fun x y -> x+y) s1 s2

let repeated_nats = map (fun x-> x/2) nats

let rec merge_helper s1 s2 k j =
  if k then 
    nth j s1
  else
    nth j s2

let rec merge s1 s2 = map2 (fun x y-> merge_helper s1 s2 (x mod 2 = 0) y) nats repeated_nats
 
let rec psums s = fby (head s) (fun () -> add (psums s) (tail s))

let max s1 s2 =  map2 (fun x y -> if x >= y then x else y) s1 s2

let rec running_max s = fby (head s) (fun () -> max (running_max s) (tail s) )


(*
 * QUESTION 2
 * 
 *)

let scalef n s = map (fun x -> (x *. n ) ) s

let divf s1 s2 = map2 (fun x y -> x/.y) s1 s2 

let inv s = map (fun x -> 1.0 /. (float_of_int x)) s 

let addf s1 s2 = map2 (fun x y -> x+.y) s1 s2

let subf s1 s2 = map2 (fun x y -> x-.y) s1 s2

let rec psumf s = fby (head s) (fun () -> addf (psumf s) (tail s))

let atan_approx k x= 
                     let coe = ((-1.0)**k) in
                     let den = (2.0*.k +. 1.0) in
                     let num = (x**den) in
                     coe*.(num/.den)   

let int_float x = map (fun z -> float_of_int z) x

let rec arctan z = psumf (map (fun x -> (atan_approx x z) ) (int_float nats))

let pi () = let at1 = (arctan (1.0/.5.0)) in
            let at2 = (arctan (1.0/.239.0)) in
            let t1 = (scalef 16.0 at1) in
            let t2 = (scalef (-4.0) at2) in
            addf t1 t2

let func f df x = divf (map f x) (map df x)

let rec newton f df y = 
  let newt x = newton f df x in
  let funcc x = func f df x in
  fby (y) (fun () -> (subf (newt y) (funcc (newt y))))

let term1  f x s = map f (addf (const x) (inv s) )

let term2 f x = map f (const x)  

let derivative f x = divf (subf (term1 f x (plus1 nats)) (term2 f x)) (inv (plus1 nats))  

let rec limit_helper mx eps s c prev = 
  if c < mx then
    let eps2 = (nth c s) -. prev in
    if (abs_float eps2) < eps then
      Some (nth c s)
    else
      (limit_helper mx eps s (c+1) (nth c s) )
  else
    None

let limit mx eps s = limit_helper mx eps s 0 ((nth 0 s)+.eps +.1.0)



(* 
 * QUESTION 3 
 * 
 *)

let rec table s1 s2 = fby ( zip (const(head s1)) s2 ) (fun () -> table (tail s1) s2 )

(* Solved this before reading hint *)

let i_jth i j s = (nth j (nth i s) )

let rec one_stripe_helper s cx cy= 
  if cx >= 0 then
    (one_stripe_helper s (cx-1) (cy+1))@[(i_jth cx cy s)]
  else
    []

let one_stripe s cx = one_stripe_helper s cx 0

let rec stripes s = map (fun x-> one_stripe s x) nats

(* let rec flatten_helper s c = map   *)

let rec flatten s = failwith "pairs not implemented"

(* fby (head s) (fun () ->  (tail s)::(flatten s)  ) *)

let pairs s1 s2 = failwith "pairs not implemented"
