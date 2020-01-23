module Residualize

open Exp
open ConfGraphs
open ExtExp

let rec graphFunEntriesRec (path: list<int>) (g: ConfGraph) : Set<list<int>> =
    match g with
    | CGFold(lvlDiff, _) -> Set.singleton (Seq.skip lvlDiff path |> List.ofSeq)
    | _ -> Set.unionMany (List.mapi (fun i g -> graphFunEntriesRec (i::path) g) (subgraphs g))

let graphFunEntries g = graphFunEntriesRec [] g

let graph2prog (g: ConfGraph) : list<ExtDef> * ExtExp =
    let entries = graphFunEntries g
    let entryName (path: list<int>) : string = sprintf "f_%s" (String.concat "_" (Seq.map string path))
    let rec loop (path: list<int>) (g: ConfGraph) : list<ExtDef> * ExtExp =
        let (defs, e) as res =
            match g with
            | CGLeaf e -> ([], exp2extExp e)
            | CGCon(c, gs) -> 
                let (defss, es) = List.unzip (List.mapi (fun i g -> loop (i::path) g) gs)
                let defs = List.concat defss
                (defs, EEApp(AKCon c, es))
            | CGUnfold g -> loop (0::path) g
            | CGCases(x, alts) ->
                let ts = List.mapi (fun i (pat, g) -> (pat, loop (i::path) g)) alts
                let (defs, alts1) = List.foldBack (fun (pat, (defs1, e)) (defs, alts) -> (defs1 @ defs, (pat, e)::alts)) ts ([], [])
                (defs, EECase(EEVar x, alts1))
            | CGFold(i, ren) -> 
                let path1 = Seq.skip i path |> List.ofSeq
                let fname = entryName path1
                let args = List.map (fun (x, y) -> EEVar y) ren
                ([], EEApp(AKFun fname, args))
            | CGLet(binds, g) ->
                let (defs, binds') = 
                    List.foldBack (fun (x, i, g) (defs, binds) ->
                        let (defs1, e) = loop (i::path) g
                        (defs1 @ defs, (x, e)::binds)) 
                        (List.mapi (fun i (x, g) -> (x, i, g)) binds)
                        ([], [])
                let (defs1, e) = loop ((List.length binds)::path) g
                (defs @ defs1, EELet(binds', e))

        if Set.contains path entries then
            let fname = entryName path
            let args = extExpVars e
            let def = (fname, args, e)
            (def::defs, EEApp(AKFun fname, List.map EEVar args))
        else res
    let (defs, e) = loop [] g
    (defs |> List.map (fun (f, xs, e) -> (f, xs, removeTrivialLets e)), removeTrivialLets e)

