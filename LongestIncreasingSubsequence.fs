open System

let rec lis l prev =
    match l with 
    | [] -> 0
    | x::xs ->
        let taken = if x > prev then lis xs x + 1 else 0
        let nottaken = lis xs prev
        Math.Max (taken, nottaken)

// lis [ 10; 9; 2; 5; 3; 7; 101; 18 ] 0

// imperative dp
let dp l =
    let subproblems = Array.create (List.length l) 0
    let mutable max = 0
    let mutable answer = 0
    match l with 
    | [] -> 0
    | _ ->
        for i in 1 .. (List.length l - 1) do 
            max <- 0
            for j in 0 .. (i-1) do 
                if List.item i l > List.item j l then 
                    max <- Math.Max(max, subproblems.[j])
            subproblems.[i] <- max + 1
            answer <- Math.Max(answer, subproblems.[i])
        answer

dp  [ 10; 9; 2; 5; 3; 7; 101; 18 ]