open System
open System.Collections.Generic;;
open System.IO

(* 
     _ _ _
    |0|1|2|
    |3|4|5|

    This file is a small implementation of the 6-puzzle game, but it could be easily augmented for n-puzzles. The purpose
    is to compare the performance of different tree search algorithms on a non-trivial problem.

    Puzzle is represented by a list, with the indexes shown above.
    I use int option to represent an entry in the puzzle, so that None means blank and Some n means tile with number n

    How to use:
        1. Download VS code and the Ionide plugin
        2. Select all the code (Cmd + a)
        3. Press Alt + Enter (this will use Ionide to compile the code)
        4. Type ``bfs searchTree;;`` in the terminal (can also call dfs, ids)

*)

//Action is defined by the direction in which the blank tile could go
type Action =
    | Up
    | Down
    | Left
    | Right

//Node of the search tree
type State =   
    {
        Id: int option list
        Parent: State option
        CostSoFar: int
        Actions: Action list
    }
with 
    static member New =
        {
            Id = []
            Parent = None
            CostSoFar = 0
            Actions = []
        }

    override x.ToString() = 
        let rec helper x i =
            match x with
            | [] -> ""
            | x::xs -> (match x with | None -> " |" | Some x -> string x + "|") + (if i = 2 then "\n|" else "") + helper xs (i+1)
        "_ _ _\n" +
        "|" + helper x.Id 0 + "\n" +
        " ¯ ¯ ¯"

type SearchTree = Node of State * SearchTree list 

let transitions (state: int option list) =
    let position = List.findIndex (fun x -> x = None) state
    match position with 
    | 0 -> [Down; Right]
    | 1 -> [Left; Down; Right]
    | 2 -> [Left; Down]
    | 3 -> [Up; Right]
    | 4 -> [Left; Up; Right]
    | 5 -> [Up; Left]
    | _ -> failwith "Impossible configuration"

let goal = 
    [
        None
        Some 1 
        Some 2 
        Some 5 
        Some 4 
        Some 3
    ]

let start = 
    [
        Some 1
        Some 4
        Some 2 
        Some 5
        Some 3 
        None
    ]

let searchTree = 
    let state = { State.New with Id = start }
    Node (state, [])

let expand state =
    let actions = transitions state.Id
    let blank = List.findIndex (fun x -> x = None) state.Id
    actions 
    |> List.map (fun action ->
        let offset =
            match action with
            | Up -> -3
            | Down -> +3
            | Left -> -1
            | Right -> +1
        let position = blank + offset 
        let elem = List.item position state.Id
        let newState =
            state.Id
            |> List.mapi (fun i x -> 
                if i = position then None elif i = blank then elem else x
            )
        { 
            Id = newState 
            Parent = Some state
            CostSoFar = state.CostSoFar + 1
            Actions = transitions newState
        }
    )

let bfs ltr = 
    let frontier = Queue<SearchTree>()
    frontier.Enqueue ltr 
    let mutable explored = Set.empty
    while (frontier.Count > 0) do 
        match frontier.Dequeue() with
        | Node (s, children) ->
            printfn "%A" (string s)
            printfn "Depth: %A" s.CostSoFar 
            if s.Id = goal then failwith "Goal found" 
            explored <- Set.add s.Id explored
            let expansion = 
                expand s
                |> List.map (fun x ->
                    Node (x, []) //each new node do not have children yet
                )
            let children = 
                expansion
                |> List.append children //children should actually be empty, but still
                |> List.sortBy (fun x -> match x with | Node (n,_) -> n) 
            for child in children do
                match child with 
                | Node (x,_) ->
                    //if x.Id = goal then failwith "Goal found"
                    if Set.contains x.Id explored then () else frontier.Enqueue(child) //pick up all the adjacent nodes

let dfs ltr =
    let mutable explored = Set.empty
    let rec helper tree =
        match tree with 
        | Node (s, children) -> 
            printfn "%A" (string s)
            printfn "Depth: %A" s.CostSoFar 
            if s.Id = goal then failwith "Goal found"
            explored <- Set.add s.Id explored 
            let expansion =
                expand s 
                |> List.map (fun x ->
                    Node (x, [])
                )
            expansion
            |> List.append children 
            |> List.sortBy (fun x -> match x with | Node (n,_) -> n) 
            |> List.iter (fun x -> 
                match x with 
                | Node (n, _) ->
                    if Set.contains n.Id explored then ()
                    else 
                        helper x
            )
    helper ltr

let ids ltr =
    let mutable explored = Set.empty
    let rec helper tree max =
        if max = 0 then ()
        else 
            match tree with 
            | Node (s, children) -> 
                printfn "%A" (string s)
                printfn "Depth: %A" s.CostSoFar 
                if s.Id = goal then failwith "Goal found"
                explored <- Set.add s.Id explored 
                let expansion =
                    expand s 
                    |> List.map (fun x ->
                        Node (x, [])
                    )
                expansion
                |> List.append children 
                |> List.sortBy (fun x -> match x with | Node (n,_) -> n) 
                |> List.iter (fun x -> 
                    match x with 
                    | Node (n, _) ->
                        if Set.contains n.Id explored then ()
                        else 
                            helper x (max-1)
                )
    let mutable l = 1
    while l <= 30 do 
        helper ltr l
        l <- l + 1
        explored <- Set.empty


        
    


