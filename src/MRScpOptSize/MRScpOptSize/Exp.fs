module Exp

type VarName = string
type ConName = string
type FunName = string

type AppKind = AKCon of ConName | AKFun of FunName

type Exp = 
    | EVar of VarName
    | EApp of AppKind * list<Exp>

let rec expVars e =
    match e with
    | EVar x -> Set.singleton x
    | EApp(_, es) -> Set.unionMany (List.map expVars es)

let rec subst (s: list<VarName * Exp>) (e: Exp) : Exp =
    match e with
    | EVar x ->
        match List.tryFind (fun (y, _) -> x = y) s with
        | Some (_, e1) -> e1
        | None -> e
    | EApp(ak, es) -> EApp(ak, List.map (subst s) es)

let freshIn (x: VarName) (xs: Set<VarName>) : VarName =
    let rec genNew (x: VarName) (i: int) (xs: Set<VarName>) : VarName =
        let x1 = (sprintf "%s%d" x i)
        if Set.contains x1 xs then genNew x (i+1) xs else x1
    genNew x 0 xs

let manyFreshIn (xs: list<VarName>) (ys: Set<VarName>) : list<VarName> =
    List.foldBack (fun x (newXs, ys) -> let newX = freshIn x ys in (newX::newXs, Set.add newX ys)) xs ([], ys)
    |> fst

let rec renamingRec (ren: list<VarName * VarName>) (e1: Exp) (e2: Exp) : option<list<VarName * VarName>> =
    let rec renamings (ren: list<VarName * VarName>) (es1: list<Exp>) (es2: list<Exp>) : option<list<VarName * VarName>> =
        match es1, es2 with
        | [], [] -> Some ren
        | e1::es1, e2::es2 -> 
            match renamingRec ren e1 e2 with
            | Some ren1 -> renamings ren1 es1 es2
            | None -> None
        | _, _ -> None
    match e1, e2 with
    | EVar x, EVar y -> 
        match List.tryFind (fun (x1, y1) -> x = x1) ren with
        | Some (_, y1) -> if y = y1 then Some ren else None
        | None -> Some ((x, y)::ren)
    | EApp(ak1, es1), EApp(ak2, es2) when ak1 = ak2 -> renamings ren es1 es2
    | _, _ -> None

let renaming e1 e2 = renamingRec [] e1 e2 |> Option.map List.rev

(* *** *)
#if false
    let rec instanceRec (s: Map<VarName, Exp>) (e1: Exp) (e2: Exp) : option<Map<VarName, Exp>> =
        let rec instances s es1 es2 = 
            match es1, es2 with
            | [], [] -> Some s
            | e1::es1, e2::es2 ->
                match instanceRec s e1 e2 with
                | Some s -> instances s es1 es2
                | None -> None
            | _, _ -> None
        match e1, e2 with
        | EVar x, _ ->
            match Map.tryFind x s with
            | None -> Some (Map.add x e2 s)
            | Some e -> if e = e2 then Some s else None
        | EApp(ak1, es1), EApp(ak2, es2) when ak1 = ak2 -> instances s es1 es2
        | _, _ -> None

    let instance e1 e2 = instanceRec Map.empty e1 e2
#endif
(* *** *)

let rec properSubexps (e: Exp) : seq<Exp * (Exp -> Exp)> =
    match e with
    | EVar _ -> Seq.empty
    | EApp(ak, es) ->
        seq {
            for (i, e) in Seq.mapi (fun i e -> (i, e)) es do
                yield (e, fun e -> EApp(ak, List.take i es @ [e] @ List.skip (i+1) es))
            for e in es do
                yield! properSubexps e }

(* *** *)

let rec homeomorphicEmbedding e1 e2 =
    let couple e1 e2 =
        match e1, e2 with
        | EVar _, EVar _ -> true
        | EApp(ak1, es1), EApp(ak2, es2) when ak1 = ak2 && List.length es1 = List.length es2 ->
            List.forall2 homeomorphicEmbedding es1 es2
        | _, _ -> false
    let embed e1 e2 =
        match e2 with
        | EVar _ -> false
        | EApp(_, es) -> List.exists (homeomorphicEmbedding e1) es
    couple e1 e2 || embed e1 e2

let homEmbTests () = 
    let c n es = EApp(AKCon n, es) in 
    let tests = [
        // true:
        (c "b" [],                   c "f" [c "b" []]);
        (c "c" [c "b" []],           c "c" [c "f" [c "b" []]]);
        (c "d" [c "b" []; c "b" []], c "d" [c "f" [c "b" []]; c "f" [c "b" []]]);
        // false:
        (c "f" [c "c" [c "b" []]],   c "c" [c "b" []]);
        (c "f" [c "c" [c "b" []]],   c "c" [c "f" [c "b" []]]);
        (c "f" [c "c" [c "b" []]],   c "f" [c "f" [c "f" [c "b" []]]])
        ]
    tests |> List.map (fun (e1, e2) -> sprintf "homEmb (%A) (%A) = %A" e1 e2 (homeomorphicEmbedding e1 e2))

(* *** *)

let antiUnify e1 e2 =
    let rec loop vars tbl e1 e2 =
        match Map.tryFind (e1, e2) tbl with
        | Some n -> (vars, tbl, EVar n)
        | None -> 
            match e1, e2 with
            | EApp(ak1, es1), EApp(ak2, es2) when ak1 = ak2 && List.length es1 = List.length es2 ->
                let (vars', tbl', es) = 
                    List.foldBack2 (fun e1 e2 (vars, tbl, es) -> 
                        let (vars', tbl', e) = loop vars tbl e1 e2
                        (vars', tbl', e::es)) 
                        es1 es2 (vars, tbl, []) 
                (vars', tbl', EApp(ak1, es))
            | _, _ ->
                let x = freshIn "x" vars
                (Set.add x vars, Map.add (e1, e2) x tbl, EVar x)
    let vars = Set.union (expVars e1) (expVars e2)
    let (_, tbl, e) = loop vars Map.empty e1 e2
    let s1 = tbl |> Map.toSeq |> Seq.map (fun ((e1, e2), x) -> (x, e1)) |> Map.ofSeq
    let s2 = tbl |> Map.toSeq |> Seq.map (fun ((e1, e2), x) -> (x, e2)) |> Map.ofSeq
    (e, s1, s2)

let antiUnifyTests () = 
    let c n es = EApp(AKCon n, es) in 
    let tests = [
        (c "b" [],                   c "f" [c "b" []]);
        (c "c" [c "b" []],           c "c" [c "f" [c "b" []]]);
        (c "c" [EVar "y"],           c "c" [c "f" [EVar "y"]]);
        (c "d" [c "b" []; c "b" []], c "d" [c "f" [c "b" []]; c "f" [c "b" []]])
        ]
    tests |> List.map (fun (e1, e2) -> sprintf "antiUnify (%A) (%A) = %A" e1 e2 (antiUnify e1 e2))

(* *** *)

type Pattern = ConName * list<VarName>

type Case = Pattern * list<VarName> * Exp

type Def =
    | FDef of FunName * list<VarName> * Exp
    | GDef of FunName * list<Case>

type Defs = list<Def>

let pat2exp (c, xs) = EApp(AKCon c, List.map EVar xs)

let lookupCase (c: ConName) (cases: list<Case>) : option<Case> =
    List.tryFind (fun ((c1, xs), ys, e) -> c = c1) cases

let defName (def: Def) : FunName =
    match def with
    | FDef(f, _, _) -> f
    | GDef(f, _) -> f

let rec lookupDef (f: FunName) (defs: Defs) : option<Def> =
    match defs with
    | [] -> None
    | def::defs -> if f = defName def then Some def else lookupDef f defs

(* *** *)

let rec simplify (defs: Defs) (unfolded: Set<FunName>) (e: Exp) : Exp =
    match e with
    | EVar x -> e
    | EApp(AKCon c, es) -> EApp(AKCon c, es |> List.map (simplify defs unfolded))
    | EApp(AKFun f, es) ->
        let es = es |> List.map (simplify defs unfolded)
        if Set.contains f unfolded then EApp(AKFun f, es)
        else
            match lookupDef f defs with
            | None -> EApp(AKFun f, es)
            | Some def ->
                let unfolded = Set.add f unfolded
                match def with
                | FDef(_, args, body) -> 
                    simplify defs unfolded (subst (List.zip args es) body)
                | GDef(_, bs) ->
                    match es with
                    | EApp(AKCon c, es1)::es2 ->
                        match lookupCase c bs with
                        | None -> EApp(AKFun f, es)
                        | Some ((_, xs), ys, body) ->
                            simplify defs unfolded (subst (List.zip xs es1 @ List.zip ys es2) body)
                    | _ -> EApp(AKFun f, es)

(* *** *)

let renameFName (ren: list<FunName * FunName>) (f: FunName) : FunName =
    match List.tryFind (fun (g, _) -> f = g) ren with
    | Some (_, f') -> f'
    | None -> f

let rec renameFuns (ren: list<FunName * FunName>) (e: Exp) : Exp =
    match e with
    | EVar _ -> e
    | EApp(AKCon c, es) -> EApp(AKCon c, es |> List.map (renameFuns ren))
    | EApp(AKFun f, es) -> EApp(AKFun (renameFName ren f), es |> List.map (renameFuns ren))

let renameDefFuns ren def =
    match def with
    | FDef(f, xs, e) -> FDef(renameFName ren f, xs, renameFuns ren e)
    | GDef(f, bs) -> GDef(renameFName ren f, bs |> List.map (fun (p, ys, e) -> (p, ys, renameFuns ren e)))

let freshenProg (fnames: Set<FunName>) (prog2: Defs * Exp) : Defs * Exp =
    let (defs2, e2) = prog2
    let fnames2 = defs2 |> List.map defName
    let (clashes, unique) = fnames2 |> List.partition (fun fname -> Set.contains fname fnames)
    let fnamesToKeep = Set.union fnames (Set.ofList unique)
    let newNames = manyFreshIn clashes fnamesToKeep
    let ren = List.zip clashes newNames
    (defs2 |> List.map (renameDefFuns ren), renameFuns ren e2)

(* *** *)

module CSE =
    open Utils

    let rec matchExps (defs: Defs) (matchingFuns: Set<FunName * FunName>) (matchingVars: Set<VarName * VarName>)
        (e1: Exp) (e2: Exp) : option<Set<FunName * FunName>> =
        let rec matchMany mFuns es1 es2 =
            match es1, es2 with
            | [], [] -> Some mFuns
            | e1::es1, e2::es2 ->
                match matchExps defs mFuns matchingVars e1 e2 with
                | None -> None
                | Some mFuns1 -> matchMany mFuns1 es1 es2
            | _, _ -> None
        let rec matchBranches mFuns bs1 bs2 =
            match bs1, bs2 with
            | [], [] -> Some mFuns
            | ((c1, xs1), ys1, e1)::bs1, ((c2, xs2), ys2, e2)::bs2 ->
                if c1 = c2 && List.length xs1 = List.length xs2 && List.length ys1 = List.length ys2
                then
                    match matchExps defs mFuns ((List.zip xs1 xs2 @ List.zip ys1 ys2) |> Set.ofList) e1 e2 with
                    | None -> None
                    | Some mFuns1 -> matchBranches mFuns1 bs1 bs2
                else None
            | _, _ -> None
        match e1, e2 with
        | EVar x, EVar y -> if Set.contains (x, y) matchingVars then Some matchingFuns else None
        | EApp(AKCon c1, es1), EApp(AKCon c2, es2) when c1 = c2 -> matchMany matchingFuns es1 es2
        | EApp(AKFun f1, es1), EApp(AKFun f2, es2) ->
            if f1 = f2 || Set.contains (f1, f2) matchingFuns then matchMany matchingFuns es1 es2
            else
                let matchingFuns = Set.add (f1, f2) matchingFuns
                match lookupDef f1 defs, lookupDef f2 defs with
                | Some (FDef(_, xs1, e1)), Some (FDef(_, xs2, e2)) when List.length xs1 = List.length xs2 ->
                    match matchExps defs matchingFuns (List.zip xs1 xs2 |> Set.ofList) e1 e2 with
                    | None -> None
                    | Some mFuns -> matchMany mFuns es1 es2
                | Some (GDef(_, bs1)), Some (GDef(_, bs2)) -> 
                    match matchBranches matchingFuns bs1 bs2 with
                    | None -> None
                    | Some mFuns -> matchMany mFuns es1 es2
                | Some _, Some _ -> None
                | _, _ -> failwithf "CSE.matchExps: missing definition of '%s' or '%s'" f1 f2
        | _, _ -> None

    let rec findRepeatedFuns (fullDefs: Defs) (defs: Defs) =
        let areDup def1 def2 =
            let f1 = defName def1
            let f2 = defName def2
            matchExps fullDefs Set.empty (Set.singleton ("x", "x")) (EApp(AKFun f1, [EVar "x"])) (EApp(AKFun f2, [EVar "x"]))
        match defs with
        | def::defs ->
            match List.tryFindSome (fun def2 -> areDup def def2) defs with
            | Some (_def2, mFuns) -> Some mFuns
            | None -> findRepeatedFuns fullDefs defs
        | _ -> None

    let splitRepeatedFuns (mFuns: Set<FunName * FunName>) =
        mFuns
        |> Set.fold (fun (keep, del, ren) (f1, f2) ->
            if Set.contains f1 keep then (keep, Set.add f2 del, (f2, f1)::ren)
            elif Set.contains f2 keep then (keep, Set.add f1 del, (f1, f2)::ren)
            else (Set.add f1 keep, Set.add f2 del, (f2, f1)::ren)) (Set.empty, Set.empty, [])

    let rec purgeRepeatedFuns (prog: Defs * Exp) : Defs * Exp =
        let (defs, e) = prog
        match findRepeatedFuns defs defs with
        | None -> prog
        | Some mFuns ->
            let (keep, del, ren) = splitRepeatedFuns mFuns
            eprintfn "split %A into %A and %A and %A" mFuns keep del ren
            let newDefs =
                []
                |> List.foldBack (fun def defs ->
                    if Set.contains (defName def) del then defs
                    else renameDefFuns ren def :: defs) defs
            let newMain = renameFuns ren e
            purgeRepeatedFuns (newDefs, newMain)

