open System

let forward x3 l =
    match l with 
    | x1::x2::_ -> x1 + x2 = x3
    | _ -> false

let backward x1 l =
    match l with 
    | x2::x3::_ -> x1 + x2 = x3
    | _ -> false

let rec subsequences l =
    match l with 
    | []
    | [_] 
    | _::[_] -> 0
    | x::y::[z] -> if x + y = z then 3 else 0
    | x::xs -> 
        let n2 = subsequences xs
        let v2 = if backward x xs then n2 + 1 else n2
        let y::ys_rev = List.rev l
        let ys = List.rev ys_rev
        let n1 = subsequences ys
        let v1 = if forward y ys_rev then n1 + 1 else n1
        Math.Max(v1, v2)