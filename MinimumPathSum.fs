open System
let grid = [
    [1; 2; 3]
    [1; 1; 3]
    [3; 1; 1]
    [1; 2; 1]
]

let M = 3
let N = 2

// not safe, but for simplicity
let grid_val m n =
    grid
    |> List.item m
    |> List.item n 

let mutable naive_calls = 0
let mutable dp_calls = 0

// naive recursive solution
let rec naive s =
    naive_calls <- naive_calls + 1
    match s with
    | (0, 0) -> grid_val 0 0
    | (0, n) -> naive (0, n-1) + grid_val 0 n
    | (m, 0) -> naive (m-1, 0)+ grid_val m 0
    | (m, n) -> Math.Min (naive (m-1, n), naive (m, n-1)) + grid_val m n


// slight modification of naive to make use of dynamic programming (top-down)
// Issue: memory
let top_down () = 
    let mutable table = Map.empty
    let rec helper (s: int * int) =
        if table.ContainsKey s then 
            table.Item s 
        else
            dp_calls <- dp_calls + 1
            let value = 
                match s with
                | (0, 0) -> grid_val 0 0
                | (0, n) -> helper (0, n-1) + grid_val 0 n
                | (m, 0) -> helper (m-1, 0)+ grid_val m 0
                | (m, n) -> Math.Min (helper (m-1, n), helper (m, n-1)) + grid_val m n
            table <- table.Add (s, value)
            value
    helper (M, N)

// in progress
let bottom_up () = 
    let mutable table = Map.empty
    let rec helper (s: int * int) =
        if table.ContainsKey s then 
            table.Item s 
        else
            dp_calls <- dp_calls + 1
            let value = 
                match s with
                | (0, n) -> helper (0, n-1) + grid_val 0 n
                | (m, 0) -> helper (m-1, 0)+ grid_val m 0
                | (m, n) -> Math.Min (helper (m+1, n), helper (m, n+1)) + grid_val m n
            table <- table.Add (s, value)
            value
    table <- table.Add ((0, 0), (grid_val 0 0))
    helper (0, 0)