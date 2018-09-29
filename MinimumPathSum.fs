open System

let grid = [
    [1; 2; 3]
    [1; 1; 3]
    [3; 1; 1]
]

let M = 2
let N = 2

// not safe
let grid_val m n =
    // printfn "m = %A n = %A" m n
    grid
    |> List.item m
    |> List.item n 

let rec naive s =
    match s with
    | (0, 0) -> grid_val 0 0
    | (0, n) -> naive (0, n-1) + grid_val 0 n
    | (m, 0) -> naive (m-1, 0)+ grid_val m 0
    | (m, n) ->
        // if m = M && n = 0 then grid_val 1 0
        // elif n = N && m = 0 then grid_val 0 1
        // else 
        let test = Math.Min (naive (m-1, n), naive (m, n-1)) + grid_val m n
        printfn "for subprob %A %A the value is %A" m n test
        Math.Min (naive (m-1, n), naive (m, n-1)) + grid_val m n