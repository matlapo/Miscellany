type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool = 
  match t with
  | Const _ -> false
  | Var y -> x = y
  | Term(f,l) -> List.exists (occurs x) l //if it's a term, apply occur to every element of the list associated with the term

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s:term) (x:id) (t: term) : term = 
  match t with 
  | Const _ -> t //constant, don't substitute it
  | Var y -> if x = y then s else t //substitute only if the right variable name
  | Term(f,l) -> Term(f,List.map (subst s x) l) //apply subst at every element of the list associated with the term

(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term):term = 

  let subst2 (s:(id * term)) (t:term) = //subst2 has 2 arguments, so we can use it with List.foldBack now
    let name,sub = s
    subst sub name t

  List.foldBack subst2 s t

(* unify one pair *)
let rec unify (s : term) (t : term) : substitution = 

  (*This returns a list of pairs and it assumes that the list are of the same length (normally I would use failwith here but I would need to use a new error
  message and I'm not sure if I'm allowed to do that so this will just crash if different length) *)
  let rec mergeLists l1 l2 = 
    match (l1,l2) with 
    | ([],[]) -> []
    | (x::xs, y::ys) -> (x,y)::(mergeLists xs ys)
    | (_,_) -> failwith "error: lists have different length"
    
  match (s,t) with
  | (Const x, Const y) -> if x = y then [] else failwith "not unifiable: clashing constants" //check if corresponding constants in both terms match
  | (Var x, Var y) -> if x = y then [] else [(x,t)] //check if the variables match
  | (Var x, Const y) -> [(x,Const y)] //subsitute the constant for the variable
  | (Const y, Var x) -> [(x,Const y)] //same thing as above
  | (Const _, Term(_)) -> failwith "not unifiable: term constant clash" // a term and a constant makes no sense for a valid substitution, so fail
  | (Term(_), Const _) -> failwith "not unifiable: term constant clash"
  | (Var x, (Term(_) as aTerm)) -> if not (occurs x aTerm) then [(x,aTerm)] else failwith "not unifiable: circularity" //if there's is not circularity, substitute t for x
  | ((Term(_) as aTerm), Var x) -> if not (occurs x aTerm) then [(x,aTerm)] else failwith "not unifiable: circularity" //same thing as above
  | (Term (f,l1), Term (g,l2)) -> //if it's 2 terms and they have the same head symbol, then unify-list them
                          if (f = g) then unify_list (mergeLists l1 l2) //since f = g, they MUST have the same arity so mergeLists will not fail
                          else failwith "not unifiable: head symbol conflict"

(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution = 
  //unify-list the rest of the list and apply the resulting substitution to each terms x and y then combine it with the remaining substitutions
  match s with 
  | [] -> []
  | (x,y)::xs -> 
            let t = unify_list xs 
            let k = unify (apply t x) (apply t y)
            k@t

(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)
