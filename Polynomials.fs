type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly = 
  
  let rec helper((c,e), p) = 
    match p with 
    | [] -> []
    | (coef,exp)::xs ->  
            (c*coef,e+exp)::helper((c,e), xs)

  match ((c,e),p) with //catch some special cases (empty poly, term with 0.0 coefficient)
  | (_,[]) -> raise EmptyList
  | ((0.0,_),_) -> Poly ([0.0,0]) //if the coefficient is 0.0, then the result is the zero poly
  | _ -> Poly(helper((c,e), p));;
             
let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 

  let rec helper((c,e), p) = 
    match p with  
    | [] -> [(c,e)]
    | (coef,exp)::xs -> //the location of the term will depend on the exposant
                if (e < exp) then (coef,exp)::helper((c,e), xs)
                elif (e = exp) then (coef+c,e)::xs
                else
                  (c,e)::(coef,exp)::xs

  match ((c,e),p) with //catch some special cases (similar to multiplyPolyByTerm)
  | (_,[]) -> raise EmptyList
  | ((0.0,_),_) -> Poly (p)
  | _ -> Poly(helper((c,e), p));;

                
let addPolys(Poly p1:poly, Poly p2:poly):poly =

  let rec helper(Poly p1:poly, Poly p2:poly) = 
    match p1 with 
    | [] -> p2  
    | x::xs -> helper(Poly(xs), addTermToPoly(Term (x), Poly (p2))) //add each terms of p1 to p2
  
  match (p1,p2) with //check if one of the polys is empty
  | ([],_) -> raise EmptyList
  | (_,[]) -> raise EmptyList
  | _ -> Poly(helper(Poly (p1), Poly (p2)));;

let multPolys(Poly p1:poly, Poly p2:poly) = 

  let rec helper(Poly p1:poly, Poly p2:poly) = 
    match p1 with 
    | [] -> Poly ([0.0,0]) //I'm aware that this adds an unecessary [0.0,0] term, I don't know how to fix this at the moment
    | x::xs -> addPolys(multiplyPolyByTerm(Term (x),Poly(p2)), helper(Poly(xs), Poly(p2))) //multiply one term by p2 and add it the multiplication of p1 (without the term) times p2

  match (p1,p2) with //check if one of the polys is empty
  | ([],_) -> raise EmptyList
  | (_,[]) -> raise EmptyList
  | _ -> helper(Poly (p1), Poly (p2));;

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0);;


let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float = 
  
  let rec helper (p,v) = 
    match p with 
    | [] -> 0.0
    | x::xs -> evalTerm (v) (Term(x)) + helper (xs,v) //eval each term recursively
  
  helper (p,v)

let diffPoly (Poly p) = 

    let rec helper l = 
        match l with 
        | [] -> []
        | (coef,exp)::xs when exp>0 -> (coef,exp-1)::(helper xs) //we don't consider negative exponents
        | (coef,exp)::xs -> helper xs

    match p with 
    | [] -> raise EmptyList
    | _ -> Poly(helper p)
