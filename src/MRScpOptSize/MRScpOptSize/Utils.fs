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

    let rec cartesian (xss: list<list<'A>>) : list<list<'A>> =
        let rec helper xs yss =
            match xs with
            | [] -> []
            | x::xs -> List.map (fun ys -> x::ys) yss @ helper xs yss
        match xss with
        | [] -> [ [] ]
        | xs::xss -> helper xs (cartesian xss)

module Seq =
    let cartesian items =
        items |> Seq.fold (fun acc s ->
            seq { for x in acc do for y in s do yield x @ [y] }) (Seq.singleton [])

module String =
    let escape s = String.collect(function '\r'->"\\r"|'\n'->"\\n"|'\t'->"\\t"|'"'->"\\\""|c->string c) s
