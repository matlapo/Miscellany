
let removeAtIndex l i k =
    let rec helper l current = 
        match l with 
        | [] -> []
        | x::xs -> if current >= i && (current - i) <= k then helper xs (current + 1) else x::xs
    helper l 0

let test = [1;2;1;2;6;7;5;1];;

removeAtIndex test 3 4;;

// let findLargest l k =
