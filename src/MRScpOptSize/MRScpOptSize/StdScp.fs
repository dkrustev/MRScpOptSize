module StdScp

open Utils
open Exp
open Driving
open ConfGraphs

type HistEntryKind = HEGlobal | HELocal

type BuildGraphResult =
    | BuiltGraph of ConfGraph
    | GeneralizeTo of int * list<VarName * Exp> * Exp

let rec buildGraphRec (defs: Defs) (nestLvl: int) (hist: list<HistEntryKind * int * Exp>) (e: Exp) : BuildGraphResult =
    let relevantHist hek hist =
        match hek with
        | HELocal -> hist |> List.takeWhile (fun (hek, _, _) -> hek = HELocal)
        | HEGlobal -> hist |> List.filter (fun (hek, _, _) -> hek = HEGlobal)
    let split e =
        match e with
        | EVar _ -> ([], e)
        | EApp(ak, es) ->
            let xs = manyFreshIn (List.replicate (List.length es) "x") (expVars e)
            (List.zip xs es, EApp(ak, List.map EVar xs))
    (*
    let simplifySubst s e =
        let (sTriv, sNontriv) = s |> List.partition (function (_, EVar _) -> true | _ -> false)
        (sNontriv, subst sTriv e)
    *)
    let rec combineResults f rs =
        let rec loop gs rs =
            match rs with
            | [] -> Choice1Of2 gs
            | BuiltGraph g :: rs -> loop (g::gs) rs
            | GeneralizeTo (lvl, binds, e') as r :: rs ->
                if lvl = nestLvl then
                    assert (subst binds e' = e)
                    //printfn "apply generalize @%d (%A) -> %A (%A)" lvl e binds e'
                    Choice2Of2 (buildGraphLet defs nestLvl hist binds e')
                else 
                    assert (lvl < nestLvl)
                    Choice2Of2 r
        match loop [] rs with
        | Choice1Of2 gs -> BuiltGraph (f (List.rev gs))
        | Choice2Of2 r -> r
    and buildGraphLet defs nestLvl hist binds e =
        //let (binds', e') = simplifySubst binds e
        let r = buildGraphRec defs (nestLvl+1) hist e
        let (xs, es) = List.unzip binds
        let rs = List.map (buildGraphRec defs (nestLvl+1) hist) es
        combineResults (fun (g::gs) -> CGLet(List.zip xs gs, g)) (r::rs)
    //printfn "enter buildGraph %d (%A)" nestLvl e
    //printfn "  with history %A" hist
    let res =
        match List.tryFindSome (fun (_, lvl, e1) -> renaming e1 e) hist with
        | Some ((_, lvl, conf1), ren) -> BuiltGraph (CGFold(nestLvl - lvl, ren))
        | None ->
            let dsr = drivingStep defs e
            let hek = match dsr with DSRCases _ -> HEGlobal | _ -> HELocal
            let relHist = relevantHist hek hist
            match List.tryFind (fun (_, _, e1) -> homeomorphicEmbedding e1 e) relHist with
            | None ->
                let hist1 = match dsr with DSRUnfold _ | DSRCases _ -> (hek, nestLvl, e)::hist | _ -> hist
                let reccall e = buildGraphRec defs (nestLvl+1) hist1 e
                match dsr with
                | DSRNone -> BuiltGraph (CGLeaf e)
                | DSRCon(c, es) -> List.map reccall es |> combineResults (fun gs -> CGCon(c, gs))
                | DSRUnfold e1 -> combineResults (fun [g] -> CGUnfold g) [reccall e1]
                | DSRCases(x, alts) -> 
                    let (ps, es) = List.unzip alts
                    let rs = List.map reccall es
                    combineResults (fun gs -> CGCases(x, List.zip ps gs)) rs
            | Some (_, lvl, e1) -> 
                printfn "whistle (%A) (%A)" e1 e
                //for (sube, f) in properSubexps e do printfn "  subexp: (%A) (%A)" sube (f (EVar "$$"))
                let (e', s1, s2) as msg = antiUnify e1 e
                printfn "msg = %A" msg
                match e' with
                | EVar _ ->
                    let (s, e'') = split e
                    buildGraphLet defs nestLvl hist s e''
                | _ -> 
                    if Map.forall (fun x e -> match e with EVar _ -> true | _ -> false) s1
                        && s1 |> Map.toSeq |> Seq.map snd |> Set.ofSeq |> Set.count = Seq.length s1
                        (* should be the same as `instance e1 e <> None` *)
                    then buildGraphLet defs nestLvl hist (Map.toList s2) e'
                    else
                        //let (s1', e'') = simplifySubst (Map.toList s1) e'
                        GeneralizeTo(lvl, Map.toList s1, e')
    //printfn "exit buildGraph %d (%A) -> %A" nestLvl e res
    res

let buildGraph defs e = 
    match buildGraphRec defs 0 [] e with
    | BuiltGraph g -> g
    | GeneralizeTo _ -> failwithf "internal error in `buildGraph (%A)`" e
