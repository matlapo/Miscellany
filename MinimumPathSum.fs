open System

// (* Custom doubly linked-list implementation *)
// type Cell = { data : int; next : RList; prev: RList }
// and RList = Cell option ref

// let head = { data = 1; next = ref None; prev = ref None }


// // let c1 = {data = 1; next = ref None}
// // let c2 = {data = 2; next = ref (Some c1)}
// // let c3 = {data = 3; next = ref (Some c2)}
// // let c5 = {data = 5; next = ref (Some c3)}

// (* This converts an RList to an ordinary list, which is then displayed. *)
// let rec displayList (c : RList) =
//   match !c with
//     | None -> []
//     | Some { data = d; next = l } -> d :: (displayList l)

// (* This converts a cell to a list.  You may find it useful for testing.  No need to
// use it in your solution. *)
// let cellToRList (c:Cell):RList = ref (Some c)


// type color = Red | Black
// type 'a rbtree =
//     Node of color * 'a * 'a rbtree * 'a rbtree | Leaf


// let rec mem x = function
//     | Leaf -> false
//     | Node (_, y, left, right) -> 
//         x = y || (x < y && mem x left) || (x > y && mem x right)


// let balance = function
//     Black, z, Node (Red, y, Node (Red, x, a, b), c), d
//   | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
//   | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
//   | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
//       Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
//   | a, b, c, d -> Node (a, b, c, d)


// let insert x s =
//   let rec ins = function
//     Leaf -> Node (Red, x, Leaf, Leaf)
//   | Node (color, y, a, b) as s ->
//       if x < y then balance (color, y, ins a, b)
//       else if x > y then balance (color, y, a, ins b)
//       else s in
//   match ins s with
//     Node (_, y, a, b) ->
//       Node (Black, y, a, b)
//     | Leaf -> (* guaranteed to be nonempty *)
//         failwith "RBT insert failed with ins returning leaf"



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
    | (m, 0) -> naive (m-1, 0) + grid_val m 0
    | (m, n) -> Math.Min (naive (m-1, n), naive (m, n-1)) + grid_val m n


// slight modification of naive to make use of memoization
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
                | (m, 0) -> helper (m-1, 0) + grid_val m 0
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
                | (m, 0) -> helper (m-1, 0) + grid_val m 0
                | (m, n) -> Math.Min (helper (m+1, n), helper (m, n+1)) + grid_val m n
            table <- table.Add (s, value)
            value
    table <- table.Add ((0, 0), (grid_val 0 0))
    helper (0, 0)