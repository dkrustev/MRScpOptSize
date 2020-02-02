module ConfGraphs

open Utils
open Exp
open Driving

type ConfGraph =
    | CGLeaf of Exp
    | CGCon of ConName * list<ConfGraph>
    | CGUnfold of ConfGraph
    | CGCases of VarName * list<Pattern * ConfGraph>
    | CGFold of int * list<VarName * VarName>
    | CGLet of list<VarName * ConfGraph> * ConfGraph

let subgraphs g =
    match g with
    | CGCon(_, gs) -> gs
    | CGUnfold g -> [g]
    | CGCases(_, alts) -> List.map snd alts
    | CGLet(defs, g) -> List.map snd defs @ [g]
    | _ -> []

let rec graphSize g =
    match g with
    | CGLeaf _ -> 1
    | CGCon(_, gs) -> 1 + List.sumBy graphSize gs
    | CGUnfold g -> 1 + graphSize g
    | CGCases(_, alts) -> 1 + List.sumBy (graphSize << snd) alts
    | CGFold _ -> 1
    | CGLet(binds, g) -> 1 + List.sumBy (graphSize << snd) binds + graphSize g

module SimpleGraph =

    let rec private buildGraphRec (defs: Defs) (maxLvl: int) (nestLvl: int) (hist: list<int * Exp>) (e: Exp) : ConfGraph =
        if nestLvl > maxLvl then CGLeaf e else
        eprintfn "buildGraph %A" e
        match List.tryFindSome (fun (lvl, e1) -> renaming e1 e) hist with
        | Some ((lvl, conf1), ren) -> CGFold(nestLvl - lvl, ren)
        | None ->
            let dsr = drivingStep defs e
            let hist1 = match dsr with DSRUnfold _ | DSRCases _ -> (nestLvl, e)::hist | _ -> hist
            let reccall = buildGraphRec defs maxLvl (nestLvl+1) hist1
            match dsr with
            | DSRNone -> CGLeaf e
            | DSRCon(c, es) -> CGCon(c, List.map reccall es)
            | DSRUnfold e -> CGUnfold(reccall e)
            | DSRCases(x, alts) -> CGCases(x, List.map (fun (pat, e) -> (pat, reccall e)) alts)

    let buildGraph defs maxLvl e = buildGraphRec defs maxLvl 0 [] e

