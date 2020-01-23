module Parsec
   
type Parser<'C, 'R> = list<'C> -> Choice<'R * list<'C>, list<string> * list<'C>>

let fail : Parser<'C, 'R> = fun cs -> Choice2Of2([], cs)

let ret (r: 'R) : Parser<'C, 'R> = fun cs -> Choice1Of2(r, cs)

let map (f: 'R1 -> 'R2) (p: Parser<'C, 'R1>) : Parser<'C, 'R2> = fun cs ->
    match p cs with
    | Choice1Of2(r, cs1) -> Choice1Of2(f r, cs1)
    | Choice2Of2(expected, cs1) -> Choice2Of2(expected, cs1)

let seq (p1: Parser<'C,'R1>) (p2: Parser<'C,'R2>) : Parser<'C,'R1 * 'R2> = fun cs ->
    match p1 cs with
    | Choice1Of2(r1, cs1) -> 
        match p2 cs1 with
        | Choice1Of2(r2, cs2) -> Choice1Of2((r1, r2), cs2)
        | Choice2Of2(expected, cs1) -> Choice2Of2(expected, cs1)
    | Choice2Of2(expected, cs1) -> Choice2Of2(expected, cs1)

let alt (p1: Parser<'C,'R>) (p2: Parser<'C,'R>) : Parser<'C,'R> = fun cs ->
    match p1 cs with
    | Choice1Of2 _ as r -> r
    | Choice2Of2(expected, cs1) -> 
        match p2 cs with
        | Choice1Of2 _ as r -> r
        | Choice2Of2(expected2, cs1) -> Choice2Of2(expected @ expected2, cs1)

let sat (pred: 'C -> bool) (expected: string) : Parser<'C, 'C> = fun cs ->
    match cs with
    | [] -> Choice2Of2([expected], cs)
    | c::cs -> if pred c then Choice1Of2(c, cs) else Choice2Of2([expected], cs)

let delay (f: unit -> Parser<'C,'R>) : Parser<'C,'R> = fun cs -> f () cs

module Operations =
    let (<*>) = seq
    let (<|>) = alt

open Operations

let optional (p: Parser<'C,'R>) : Parser<'C, option<'R>> =
    p |> map Some <|> ret None

let rec many (p: Parser<'C,'R>) : Parser<'C, list<'R>> = 
    delay (fun () -> many1 p <|> ret [])
and many1 (p: Parser<'C,'R>) : Parser<'C, list<'R>> = 
    delay (fun () -> p <*> many p |> map (fun (r, rs) -> r::rs))

let sepBy1 (elP: Parser<'C,'R>) (sepP: Parser<'C,'R1>) : Parser<'C,list<'R>> =
    elP <*> many (sepP <*> elP |> map snd) |> map (fun (r, rs) -> r::rs)

let sepBy (elP: Parser<'C,'R>) (sepP: Parser<'C,'R1>) : Parser<'C,list<'R>> =
    sepBy1 elP sepP <|> ret []

let between pDel1 p pDel2 = pDel1 <*> p <*> pDel2 |> map (fun ((_, x), _) -> x)

