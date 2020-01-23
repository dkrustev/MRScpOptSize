module Utils

module List =
    let rec tryFindSome (f: 'a -> option<'b>) (xs: list<'a>) : option<'a * 'b> =
        match xs with
        | [] -> None
        | x::xs -> 
            match f x with
            | Some r -> Some (x, r)
            | None -> tryFindSome f xs

    let rec removeDuplicates xs =
        match xs with
        | [] -> []
        | x::xs -> if List.exists (fun y -> x = y) xs then removeDuplicates xs else x::removeDuplicates xs

    let rec difference xs ys =
        match xs with
        | [] -> []
        | x::xs -> if List.exists (fun y -> x = y) ys then difference xs ys else x::difference xs ys

