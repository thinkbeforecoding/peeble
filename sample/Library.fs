namespace sample

module Say =
    let hello name =
        sprintf "Hello from Php %s" name

module Test = 
    let rec filterEven l =
        match l with
        | x :: tail when x % 2 = 0 -> x :: filterEven tail
        | _ :: tail -> filterEven tail
        | [] -> []
        
