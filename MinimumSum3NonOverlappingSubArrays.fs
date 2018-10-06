let removeAtIndex l i j =
    l 
    |> List.indexed
    |> List.filter (fun (index, _) -> index < i || index > (i+j-1))
    |> List.map snd

let test = [1;2;1;2;6;7;5;1];;

removeAtIndex test 3 2;;

