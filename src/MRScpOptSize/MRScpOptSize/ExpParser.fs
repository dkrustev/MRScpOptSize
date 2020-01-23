module ExpParser

open Exp
open Parsec
open Operations

let list2str cs = System.String(List.toArray cs)

let char c = sat (fun c1 -> c = c1) (System.String([|c|]))

let lowercase = sat (fun c -> 'a' <= c && c <= 'z') "lowercase letter"

let uppercase = sat (fun c -> 'A' <= c && c <= 'Z') "uppercase letter"

let letter = sat (fun c -> 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z') "letter"

let digit = sat (fun c -> '0' <= c && c <= '9') "digit"

let whitespace = many (sat (fun c -> System.Char.IsWhiteSpace(c)) "whitespace")

let charWS c = char c <*> whitespace |> map fst

let lcIdent = lowercase <*> many (letter <|> digit) |> map (fun (c, s) -> list2str (c::s))

let lcIdentWS = lcIdent <*> whitespace |> map fst

let ucIdent = uppercase <*> many (letter <|> digit) |> map (fun (c, s) -> list2str (c::s))

let ucIdentWS = ucIdent <*> whitespace |> map fst

let rec exp (*: Parser<char, Exp> *) = delay (fun () ->
    (lcIdentWS <*> optional (between (charWS '(') (sepBy expWS (charWS ',')) (charWS ')')) |> map (fun (id, ot) -> match ot with Some es -> EApp(AKFun id, es) | None -> EVar id))
    <|> (ucIdentWS <*> optional (between (charWS '(') (sepBy expWS (charWS ',')) (charWS ')')) |> map (fun (id, ot) -> match ot with Some es -> EApp(AKCon id, es) | None -> EApp(AKCon id, [])))
    )
and expWS = delay (fun () -> exp <*> whitespace |> map fst)

let pat = ucIdentWS <*> optional (charWS '(' <*> sepBy lcIdentWS (charWS ',') <*> charWS ')') |> map (fun (id, ot) -> (id, match ot with Some((_,xs),_) -> xs | None -> []))

let paramList = (pat |> map Choice1Of2 <|> (lcIdentWS |> map Choice2Of2)) <*> many (charWS ',' <*> lcIdentWS |> map snd)

let defLine = lcIdentWS <*> between (charWS '(') paramList (charWS ')') <*> between (charWS '=') exp (charWS ';') |> map (fun ((id, pars), e) -> (id, (pars, e)))
        
let defLines = whitespace <*> many1 defLine |> map snd

let defLines2defs dls =
    let groups = Seq.groupBy fst dls |> Seq.map (fun (id, dls) -> (id, Seq.map snd dls |> Seq.toList))
    let classify defs =
        List.foldBack (fun ((par1, pars), e) cls -> 
            match cls, par1 with
            | None, Choice2Of2 x -> Some (Choice2Of3(x::pars, e))
            | None, Choice1Of2 pat -> Some (Choice1Of3 [(pat, pars, e)])
            | Some (Choice1Of3 alts), Choice1Of2 pat -> Some (Choice1Of3 ((pat, pars, e)::alts))
            | _, _ -> Some (Choice3Of3 ())
            ) defs None 
    groups
    |> Seq.fold (fun (defs, bad) (fname, dfs) ->
        match classify dfs with
        | Some (Choice1Of3 alts) -> (GDef(fname, alts)::defs, bad)
        | Some (Choice2Of3 (pars, e)) -> (FDef(fname, pars, e)::defs, bad)
        | _ -> (defs, (fname, dfs)::bad)
        ) ([], [])

let str2exp (s: string) : Exp =
    match exp (List.ofSeq s) with
    | Choice1Of2(e, []) -> e
    | Choice1Of2(_, rest) -> failwithf "unparsed input: %s" (list2str rest)
    | Choice2Of2(expected, rest) -> failwithf "syntax error - expecting one of %A here: %s" expected (list2str rest)

let str2defs (s: string) =
    match defLines (List.ofSeq s) with
    | Choice1Of2(dls, []) -> 
        match defLines2defs dls with
        | (defs, []) -> defs
        | (_, bad) -> failwithf "bad definitions: %A" bad
    | Choice1Of2(_, rest) -> failwithf "unparsed input: %s" (list2str rest)
    | Choice2Of2(expected, rest) -> failwithf "syntax error - expecting one of %A here: %s" expected (list2str rest)
