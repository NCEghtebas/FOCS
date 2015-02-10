(* FoCS Spring 2015

   Homework 1 code


   Name: Chloe Eghtebas 

   Email: nassim.eghtebas@students.olin.edu

   Comments: Fun!

 *)

(* 
 *  Question 1
 *)
let rec addOne xs=
  match xs with
  | []-> []
  | (hd::tl)->(hd+1)::(addOne tl)

(* Reverses order of elements in a List *)
let rec reverse (xs, ret) =
  match xs with
  | [] -> ret
  | (hd::tl) -> reverse( tl , (hd::ret) )

(* Helper function because I don't want to keep reversing xs*)
let rec appendHelp (xs, ys) =
  match xs with 
    | [] -> ys
    | (hd::tl) -> appendHelp (tl, hd::ys)

(* Working Append function *)
let rec append (xs,ys) =   
  let sx = reverse(xs, []) in
    appendHelp (sx, ys)

(* Flattens a list of lists into a single list *)
let rec flatten (xs) = 
  match xs with 
  | [] -> []
  | (hd::tl) -> append(hd, flatten(tl)) 

(* Doubles int elements in a list *)
let rec double (xs) = 
  match xs with 
  | [] -> []
  | (hd::tl) -> ((hd*2) :: (double (tl) ))

(* Returns last element of list *)
let rec last (xs) = 
  match xs with
  | [] -> None
  | (hd::[]) -> Some hd
  | (hd::tl) -> last(tl)

(*
 *  Question 2 
 *)

(* Returns true if elemnt is in set, flase if not *)
let rec setIn (elt,set) = 
  match set with
  | [] -> false
  | (hd::tl) -> if elt = hd then
                  true
                else 
                  setIn(elt, tl)

(* Returns true if set1 is cointained in set 2, false if not *)
let rec setSub (set1,set2) = 
  match set1 with
  | [] -> true
  | (hd::tl) -> if setIn(hd, set2) = true then
                  setSub(tl, set2)
                else
                  false

(* Returns true if set1 = set2 *)
let setEqual (set1,set2) = 
  setSub(set1, set2) && setSub(set2, set1)

(* Returns only unique elements in the set *)
let rec unRepeat (xs) =
  match xs with
  | [] -> []
  | (hd::tl) -> if setIn(hd, tl) then
                  unRepeat(tl)
                else
                  (hd::unRepeat(tl))

(* Returns the union of two sets *)
let setUnion (set1,set2) = 
  unRepeat(append(set1, set2))

(* Returns the intersection of two sets *)
let rec setInter (set1,set2) = 
  match set1 with 
  | [] -> []
  | (hd::tl) -> if setIn(hd, set2) then
                  hd::setInter(tl, set2)
                else
                  setInter(tl, set2)

(* Returns the number of unique elements in a set  *)
let rec setSize (set) =
  match unRepeat(set) with
  | [] -> 0
  | (hd::tl) -> 1+setSize(tl)

(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

(* Finds the gcd *)
let rec gcd(a, b) =
  match (a,b) with
  | (a,0) -> a
  | (a,b) -> gcd(b, a mod b)

(* simplifies a faction of integers *)
let simplify (r) = 
  {num=r.num/gcd(r.num, r.den); den=r.den/gcd(r.num, r.den)}

(* Adds two rational numbers and resurns the simplest form *)
let addR (r1,r2) =
  if r1.den = r2.den then 
    simplify({num = r1.num + r2.num ; den = r1.den})
  else
    simplify({num= r1.num * r2.den + r2.num * r1.den ; den = r1.den *r2.den})

(* Multiplies two rational numbers *)
let multR (r1,r2) = 
  simplify({num = r1.num * r2.num ; den = r1.den * r2.den})

type number = I of int
            | R of rat
            | F of float

let intToR (n) =
  {num = n; den = 1}

(* Adds float, rationals, and ints *)
let add (n1,n2) = 
  match (n1, n2) with
  | (I n1, I n2) -> I (n1+n2)
  | (I n1, R n2) -> R (addR(intToR(n1), n2))
  | (I n1, F n2) -> F (float(n1) +. n2)
  | (R n1, I n2) -> R (addR(n1, intToR(n2)))
  | (R n1, R n2) -> R (addR(n1, n2))
  | (R n1, F n2) -> F (floatR(n1) +. n2)
  | (F n1, I n2) -> F (n1 +. float(n2))
  | (F n1, R n2) -> F (n1 +. floatR(n2))
  | (F n1, F n2) -> F (n1 +. n2)

(* 
 *  Optional question
 *)

type bConst = True | False

type bExpr = Constant of bConst
           | Variable of string
           | And of bExpr * bExpr
           | Or of bExpr * bExpr
           | Not of bExpr

let sample1 = And(Not(Variable "a"),Not(Variable "b"))

let sample2 = Or(Not(Variable "a"),And(Variable "b",Constant(True)))

let sample3 = And(Variable "a", Not(Variable "a"))


let rec vars (bexpr) = 
  match bexpr with
  | Constant _ -> []
  | Variable (v1) -> [v1]
  | And (v1, v2) -> unRepeat(flatten([vars (v1); vars(v2)]))
  | Not (v1) -> vars(v1)
  | Or (v1, v2) -> unRepeat(flatten([vars (v1); vars (v2)]))
  

let rec subst (bexpr,var,sub) = 
  match bexpr with 
  | Constant _ -> bexpr
  | Variable (v1) -> if v1 = var then
                      sub
                     else
                      bexpr
  | Not (v1) -> Not(subst(v1, var, sub))
  | And (v1, v2) -> And(subst(v1, var, sub), subst(v2, var, sub))
  | Or (v1, v2) -> Or(subst(v1, var, sub), subst(v2, var, sub))


let rec eval (bexpr) = 
  match bexpr with
  | Constant x -> Some x
  | Variable x -> None
  | Not x -> (match eval(x) with
             | None -> None
             | Some True -> Some False
             | Some False -> Some True)
  | Or (x1, x2) -> (match (eval(x1), eval(x2)) with
                    | (None, _) -> None
                    | (_, None) -> None 
                    | (Some True, _) -> Some True
                    | (_, Some True) -> Some True
                    | (Some False, Some False) -> Some False)
  | And (x1, x2) -> (match (eval(x1), eval(x2)) with
                     | (None, _) -> None
                     | (_, None) -> None
                     | (Some False, _) -> Some False
                     | (_, Some False) -> Some False
                     | (Some True, Some True) -> Some True 
                     ) 
















