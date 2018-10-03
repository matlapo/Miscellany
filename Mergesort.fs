let rec split l = 
  match l with
  | x1::(x2::xs) -> 
                let (a,b) = split xs
                (x1::a,x2::b)
  | x::xs -> ([x],[])
  | [] -> ([],[])

let rec merge twolists =
    match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> [x]
    | ([],x::xs) -> [x]
    | (x::xs,y::ys) -> if x < y then  x::merge (xs,y::ys)
                        else y::merge (x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] // removing this => infinite loop
  | n::ns -> 
        let a,b = split l
        merge (mergesort a,mergesort b)



