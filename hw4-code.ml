(* FoCS Spring 2015

   Homework 4 code


   Name: Chloe

   Email: nassim.eghtebas@students.olin.edu

   Comments: If I had more time I would have liked to do all 4.

 *)



(*
 *
 *  QUESTION 1 
 * 
 *
 *)


let rec last l = 
  match l with
    [] -> None
  | [x] -> Some(x)
  | x::xs -> last(xs)

(* Makes non-optional returing boolean function with one input return optional *)
let predicate_opt p = 
  fun x -> 
    match p x with
    | false -> None
    | true -> Some x

(* Makes non-optional returing function with one input return optional *)
let map_opt f = 
  fun x ->
    match x with
     | None -> None
     | Some y -> Some (f y)

(* Makes non-optional returing function with two inputs return optional *)
let comb_opt f = 
  (fun x y -> 
    match x with
    | None -> None
    | Some c -> 
      (match y with 
        |None -> None
        |Some c2 -> Some (f c c2))
  )

(* Returns first argument if second argument optional in None *)
let default v o = 
    match o with
    | None -> v
    | Some c -> c

(* Returns the composite of input functions f and g as an optional*)
let compose_opt f g = 
(fun x -> 
  match f x with
  | None -> None
  | Some c -> 
    (match g c with
     | None -> None
     | Some c2 -> Some c2 
    )
)

(*
 * 
 * QUESTION 2 
 * 
 * 
 *)

(* at least it works *)
let at_least n p xs = (List.fold_left (fun l res -> l+res) 0
  (List.map (fun x-> if x then 1 else 0 ) (List.map p xs))) >=n

let find_greater num onum = (match onum with 
                             | None -> Some num
                             | Some o -> if o>= num then
                                            Some o
                                         else
                                            Some num
                            )

let max_list xs = (List.fold_right (fun l res-> (find_greater l res)) xs None)

let map_funs fs x = (List.map (fun f -> f x) fs)

let map_cross fs xs = ( List.flatten (List.map (fun x -> map_funs fs x) xs) )



(*
 * 
 * QUESTION 3
 * 
 * 
 *)


let suffixes xs = (List.fold_right (fun l res-> (l::(List.hd res))::res ) xs [[]] )

let prefixes xs = (List.fold_left (fun res l-> ( res@[(List.hd (List.rev res))@[l]] ) ) [[]] xs )

let inject a xs = (List.map2 (fun p s-> ((p@[a])@s) ) (prefixes xs) (suffixes xs) )

(* removes element at index *)
(* let index_blah i xs= (List.flatten (List.mapi (fun j x -> (if j = i then [] else [x])) xs) ) *)


let perms_helper i xs = (List.flatten( List.map (fun x -> (inject i x)) xs ) )

let perms xs =  (List.fold_right (fun l res -> (perms_helper l res)) xs [[]]) 

  (* (List.map (fun a -> (inject a xs)) xs )  *)




(*
 * 
 * QUESTION 4
 * 
 * 
 *)


type 'a bintree = 
    EmptyTree 
  | Node of 'a * 'a bintree * 'a bintree

let sample = 
  Node (10, Node (3, Node (7, EmptyTree, EmptyTree),
                     Node (5, EmptyTree, EmptyTree)),
            Node (6, Node (99, EmptyTree, 
                               Node (66, EmptyTree, EmptyTree)),
                     EmptyTree))
                                 
let print_tree t = 
  let print_typ_tree f t = 
    let emptyString n = String.make n ' '  in
    let ljustify n s = s ^ (emptyString (n - (String.length s)))  in
    let height p = List.length p  in
    let width p = List.fold_right (fun s m -> max (String.length s) m) p 0  in
    let rec copy n x = 
      if (n <= 0)
        then []
      else x :: copy (n - 1) x  in
    let empty h w = copy h (emptyString w)  in
    let above p q = 
      let w = max (width p) (width q)
      in (List.map (ljustify w) p) @ (List.map (ljustify w) q)  in
    let beside p q = 
      let h = max (height p) (height q)  in
      let heighten h p = above p (empty (h - List.length p) (width p))
      in List.map2 (^) (heighten h p) (heighten h q)  in
    let string_picture p = (String.concat "\n" p)^"\n"  in
    let print_picture p = Printf.printf "%s" (string_picture p)  in
    let rec picture_tree f t = 
      match t with
        EmptyTree -> [" "]
      | Node (v,EmptyTree,EmptyTree) -> [f v]
      | Node (v,EmptyTree,r) -> above [f v]
            (above ["---|"]
               (beside ["   "] (picture_tree f r)))
      | Node (v,l,EmptyTree) -> above [f v]
            (above ["|"] 
               (picture_tree f l))
      | Node (v,l,r) -> let sub_l = picture_tree f l in
        let sub_r = picture_tree f r
        in above [f v]
          (above ["|"^(String.make (2 + width sub_l) '-')^"|"]
             (beside sub_l (beside ["   "] sub_r)))
    in print_picture (picture_tree f t) in
  print_typ_tree string_of_int t


let rec mapT f t = 
  match t with
    EmptyTree -> EmptyTree
  | Node (v,l,r) -> Node (f v, mapT f l, mapT f r)

let rec foldT f t b = 
  match t with
    EmptyTree -> b
  | Node (v,l,r) -> f v (foldT f l b) (foldT f r b)

let size t = foldT (fun v l r -> 1 + l + r) t 0
let sum t = foldT (fun v l r -> v + l + r) t 0


let height t = failwith "not implemented"

let height' t = failwith "not implemented"

let fringe t = failwith "not implemented"

let fringe' t = failwith "not implemented"


let inorder t = failwith "not implemented"

let preorder t = failwith "not implemented"

let postorder t = failwith "not implemented"


let make_tree xs = failwith "not implemented"


