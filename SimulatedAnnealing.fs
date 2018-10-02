let Y (x: float) = sin (x*x/2.0) / (log10(x+4.0)/log10(10.0))

let best x y = if Y x > Y y then (x, Y x) else (y, Y y)

let generate x delta = 
    let a = if x + delta > 10.0 then None else Some (x + delta)
    let b = if x - delta < 0.0 then None else Some (x - delta)
    (a, b) 

let randomGenerate x delta = //either goes right of left by an amount of delta
        let n = System.Random().Next 10
        if n > 5 then 
            if x + delta > 10.0 then x - delta else x + delta
        else 
            if x - delta < 0.0 then x + delta else x - delta



let rec climb x delta step : (float*float) list = 
    let t = generate x delta 
    match t with 
    | (Some a, Some b) -> 
        let m = best (best a b |> fst) x //find overall best
        if fst m = x then 
            printfn "Reached %A with %A steps" m step
            [m] //we found a max, stop
        else 
            List.append [m] (climb (fst m) delta (step+1)) //keep climbing
    | (Some a, None) | (None, Some a) -> 
        let m = best a x 
        if fst m = x then 
            printfn "Reached %A with %A steps" m step
            [m] 
        else 
            List.append [m] (climb (fst m) delta (step+1)) 
    | (None, None) -> failwith "Impossible scenario"

let alpha = 0.2

let random = System.Random()

let rec simAnnealing x xmax delta temp steps =
    let t = randomGenerate x delta 
    let xmax = if Y t > Y xmax then t else xmax
    if Y t > Y x then 
        random.Next () |> ignore
        List.append [t] (simAnnealing t t delta (temp * alpha) (steps+1))
    else 
        let p = exp (-(Y x - Y t)/temp)
        if p < 0.0000001 then 
            [x]
        else
            let ran = random.Next 100 |> float
            if ran < (100.0 * p) then 
                List.append [t] (simAnnealing t xmax delta (temp * alpha) (steps+1))
            else 
                List.append [x] (simAnnealing x xmax delta (temp * alpha) (steps+1))